/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                            R A I S E - G C C                             *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *             Copyright (C) 1992-2013, Free Software Foundation, Inc.      *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* Code related to the integration of the GCC mechanism for exception
   handling.  */

#ifndef IN_RTS
#error "RTS unit only"
#endif

#include "tconfig.h"
#include "tsystem.h"

#include <stdarg.h>
typedef char bool;
# define true 1
# define false 0

#include "raise.h"

#ifdef __APPLE__
/* On MacOS X, versions older than 10.5 don't export _Unwind_GetIPInfo.  */
#undef HAVE_GETIPINFO
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 1050
#define HAVE_GETIPINFO 1
#endif
#endif

#if defined (__hpux__) && defined (USE_LIBUNWIND_EXCEPTIONS)
/* HP-UX B.11.31 ia64 libunwind doesn't have _Unwind_GetIPInfo. */
#undef HAVE_GETIPINFO
#define _UA_END_OF_STACK 0
#endif

/* The names of a couple of "standard" routines for unwinding/propagation
   actually vary depending on the underlying GCC scheme for exception handling
   (SJLJ or DWARF). We need a consistently named interface to import from
   a-except, so wrappers are defined here.  */

#include "unwind.h"

typedef struct _Unwind_Context _Unwind_Context;
typedef struct _Unwind_Exception _Unwind_Exception;

_Unwind_Reason_Code
__gnat_Unwind_RaiseException (_Unwind_Exception *);

_Unwind_Reason_Code
__gnat_Unwind_ForcedUnwind (_Unwind_Exception *, void *, void *);

extern struct Exception_Occurrence *__gnat_setup_current_excep
 (_Unwind_Exception *);
extern void __gnat_unhandled_except_handler (_Unwind_Exception *);

#include "unwind-pe.h"

/* The known and handled exception classes.  */

#define CXX_EXCEPTION_CLASS 0x474e5543432b2b00ULL
#define GNAT_EXCEPTION_CLASS 0x474e552d41646100ULL

/* --------------------------------------------------------------
   -- The DB stuff below is there for debugging purposes only. --
   -------------------------------------------------------------- */

#ifndef inhibit_libc

#define DB_PHASES     0x1
#define DB_CSITE      0x2
#define DB_ACTIONS    0x4
#define DB_REGIONS    0x8

#define DB_ERR        0x1000

/* The "action" stuff below is also there for debugging purposes only.  */

typedef struct
{
  _Unwind_Action phase;
  const char * description;
} phase_descriptor;

static const phase_descriptor phase_descriptors[]
  = {{ _UA_SEARCH_PHASE,  "SEARCH_PHASE" },
     { _UA_CLEANUP_PHASE, "CLEANUP_PHASE" },
     { _UA_HANDLER_FRAME, "HANDLER_FRAME" },
     { _UA_FORCE_UNWIND,  "FORCE_UNWIND" },
     { -1, 0}};

static int
db_accepted_codes (void)
{
  static int accepted_codes = -1;

  if (accepted_codes == -1)
    {
      char * db_env = (char *) getenv ("EH_DEBUG");

      accepted_codes = db_env ? (atoi (db_env) | DB_ERR) : 0;
      /* Arranged for ERR stuff to always be visible when the variable
	 is defined. One may just set the variable to 0 to see the ERR
	 stuff only.  */
    }

  return accepted_codes;
}

#define DB_INDENT_INCREASE 0x01
#define DB_INDENT_DECREASE 0x02
#define DB_INDENT_OUTPUT   0x04
#define DB_INDENT_NEWLINE  0x08
#define DB_INDENT_RESET    0x10

#define DB_INDENT_UNIT     8

static void
db_indent (int requests)
{
  static int current_indentation_level = 0;

  if (requests & DB_INDENT_RESET)
    current_indentation_level = 0;

  if (requests & DB_INDENT_INCREASE)
    current_indentation_level ++;

  if (requests & DB_INDENT_DECREASE)
    current_indentation_level --;

  if (requests & DB_INDENT_NEWLINE)
    fprintf (stderr, "\n");

  if (requests & DB_INDENT_OUTPUT)
    fprintf (stderr, "%*s", current_indentation_level * DB_INDENT_UNIT, " ");
}

static void ATTRIBUTE_PRINTF_2
db (int db_code, char * msg_format, ...)
{
  if (db_accepted_codes () & db_code)
    {
      va_list msg_args;

      db_indent (DB_INDENT_OUTPUT);

      va_start (msg_args, msg_format);
      vfprintf (stderr, msg_format, msg_args);
      va_end (msg_args);
    }
}

static void
db_phases (int phases)
{
  const phase_descriptor *a = phase_descriptors;

  if (! (db_accepted_codes() & DB_PHASES))
    return;

  db (DB_PHASES, "\n");

  for (; a->description != 0; a++)
    if (phases & a->phase)
      db (DB_PHASES, "%s ", a->description);

  db (DB_PHASES, " :\n");
}
#else /* !inhibit_libc */
#define db_phases(X)
#define db_indent(X)
#define db(X, ...)
#endif /* !inhibit_libc */

/* ---------------------------------------------------------------
   --  Now come a set of useful structures and helper routines. --
   --------------------------------------------------------------- */

/* There are three major runtime tables involved, generated by the
   GCC back-end. Contents slightly vary depending on the underlying
   implementation scheme (dwarf zero cost / sjlj).

   =======================================
   * Tables for the dwarf zero cost case *
   =======================================

   They are fully documented in:
     http://sourcery.mentor.com/public/cxx-abi/exceptions.pdf
   Here is a shorter presentation, with some specific comments for Ada.

   call_site []
   -------------------------------------------------------------------
   * region-start | region-length | landing-pad | first-action-index *
   -------------------------------------------------------------------

   Identify possible actions to be taken and where to resume control
   for that when an exception propagates through a pc inside the region
   delimited by start and length.

   A null landing-pad indicates that nothing is to be done.

   Otherwise, first-action-index provides an entry into the action[]
   table which heads a list of possible actions to be taken (see below).

   If it is determined that indeed an action should be taken, that
   is, if one action filter matches the exception being propagated,
   then control should be transfered to landing-pad.

   A null first-action-index indicates that there are only cleanups
   to run there.

   action []
   -------------------------------
   * action-filter | next-action *
   -------------------------------

   This table contains lists (called action chains) of possible actions
   associated with call-site entries described in the call-site [] table.
   There is at most one action list per call-site entry.  It is SLEB128
   encoded.

   A null action-filter indicates a cleanup.

   Non null action-filters provide an index into the ttypes [] table
   (see below), from which information may be retrieved to check if it
   matches the exception being propagated.

   * action-filter > 0:
   means there is a regular handler to be run The value is also passed
   to the landing pad to dispatch the exception.

   * action-filter < 0:
   means there is a some "exception_specification" data to retrieve,
   which is only relevant for C++ and should never show up for Ada.
   (Exception specification specifies which exceptions can be thrown
   by a function. Such filter is emitted around the body of C++
   functions defined like:
     void foo ([...])  throw (A, B) { [...] }
   These can be viewed as negativ filter: the landing pad is branched
   to for exceptions that doesn't match the filter and usually aborts
   the program).

   * next-action
   points to the next entry in the list using a relative byte offset. 0
   indicates there is no other entry.

   ttypes []
   ---------------
   * ttype-value *
   ---------------

   This table is an array of addresses.

   A null value indicates a catch-all handler.  (Not used by Ada)

   Non null values are used to match the exception being propagated:
   In C++ this is a pointer to some rtti data, while in Ada this is an
   exception id (with a fake id for others).

   For C++, this table is actually also used to store "exception
   specification" data. The differentiation between the two kinds
   of entries is made by the sign of the associated action filter,
   which translates into positive or negative offsets from the
   so called base of the table:

   Exception Specification data is stored at positive offsets from
   the ttypes table base, which Exception Type data is stored at
   negative offsets:

   ---------------------------------------------------------------------------

   Here is a quick summary of the tables organization:

	  +-- Unwind_Context (pc, ...)
	  |
	  |(pc)
	  |
	  |   CALL-SITE[]
	  |
	  |   +=============================================================+
	  |   | region-start + length |  landing-pad   | first-action-index |
	  |   +=============================================================+
	  +-> |       pc range          0 => no-action   0 => cleanups only |
	      |                         !0 => jump @              N --+     |
	      +====================================================== | ====+
                                                                      |
                                                                      |
       ACTION []                                                      |
                                                                      |
       +==========================================================+   |
       |              action-filter           |   next-action     |   |
       +==========================================================+   |
       |  0 => cleanup                                            |   |
       | >0 => ttype index for handler ------+  0 => end of chain | <-+
       | <0 => ttype index for spec data     |                    |
       +==================================== | ===================+
                                             |
                                             |
       TTYPES []                             |
					     |  Offset negated from
		 +=====================+     |  the actual base.
		 |     ttype-value     |     |
    +============+=====================+     |
    |            |        ...          |     |
    |    ...     |     exception id    | <---+
    |            |        ...          |
    |  handlers	 +---------------------+
    |            |        ...          |
    |    ...     |        ...          |
    |            |        ...          |
    +============+=====================+ <<------ Table base
    |    ...     |        ...          |
    |   specs    |        ...          | (should not see negative filter
    |    ...     |        ...          |  values for Ada).
    +============+=====================+


   ============================
   * Tables for the sjlj case *
   ============================

   So called "function contexts" are pushed on a context stack by calls to
   _Unwind_SjLj_Register on function entry, and popped off at exit points by
   calls to _Unwind_SjLj_Unregister. The current call_site for a function is
   updated in the function context as the function's code runs along.

   The generic unwinding engine in _Unwind_RaiseException walks the function
   context stack and not the actual call chain.

   The ACTION and TTYPES tables remain unchanged, which allows to search them
   during the propagation phase to determine whether or not the propagated
   exception is handled somewhere. When it is, we only "jump" up once directly
   to the context where the handler will be found. Besides, this allows "break
   exception unhandled" to work also

   The CALL-SITE table is setup differently, though: the pc attached to the
   unwind context is a direct index into the table, so the entries in this
   table do not hold region bounds any more.

   A special index (-1) is used to indicate that no action is possibly
   connected with the context at hand, so null landing pads cannot appear
   in the table.

   Additionally, landing pad values in the table do not represent code address
   to jump at, but so called "dispatch" indices used by a common landing pad
   for the function to switch to the appropriate post-landing-pad.

   +-- Unwind_Context (pc, ...)
   |
   | pc = call-site index
   |  0 => terminate (should not see this for Ada)
   | -1 => no-action
   |
   |   CALL-SITE[]
   |
   |   +=====================================+
   |   |  landing-pad   | first-action-index |
   |   +=====================================+
   +-> |                  0 => cleanups only |
       | dispatch index             N        |
       +=====================================+


   ===================================
   * Basic organization of this unit *
   ===================================

   The major point of this unit is to provide an exception propagation
   personality routine for Ada. This is __gnat_personality_v0.

   It is provided with a pointer to the propagated exception, an unwind
   context describing a location the propagation is going through, and a
   couple of other arguments including a description of the current
   propagation phase.

   It shall return to the generic propagation engine what is to be performed
   next, after possible context adjustments, depending on what it finds in the
   traversed context (a handler for the exception, a cleanup, nothing, ...),
   and on the propagation phase.

   A number of structures and subroutines are used for this purpose, as
   sketched below:

   o region_descriptor: General data associated with the context (base pc,
     call-site table, action table, ttypes table, ...)

   o action_descriptor: Data describing the action to be taken for the
     propagated exception in the provided context (kind of action: nothing,
     handler, cleanup; pointer to the action table entry, ...).

   raise
     |
    ... (a-except.adb)
     |
   Propagate_Exception (a-exexpr.adb)
     |
     |
   _Unwind_RaiseException (libgcc)
     |
     |   (Ada frame)
     |
     +--> __gnat_personality_v0 (context, exception)
	   |
	   +--> get_region_description_for (context)
	   |
	   +--> get_action_description_for (ip, exception, region)
	   |       |
	   |       +--> get_call_site_action_for (context, region)
	   |            (one version for each underlying scheme)
           |
	   +--> setup_to_install (context)

   This unit is inspired from the C++ version found in eh_personality.cc,
   part of libstdc++-v3.

*/


/* This is an incomplete "proxy" of the structure of exception objects as
   built by the GNAT runtime library. Accesses to other fields than the common
   header are performed through subprogram calls to alleviate the need of an
   exact counterpart here and potential alignment/size issues for the common
   header. See a-exexpr.adb.  */

typedef struct
{
  _Unwind_Exception common;
  /* ABI header, maximally aligned. */
} _GNAT_Exception;

/* The two constants below are specific ttype identifiers for special
   exception ids.  Their type should match what a-exexpr exports.  */

extern const int __gnat_others_value;
#define GNAT_OTHERS      ((_Unwind_Ptr) &__gnat_others_value)

extern const int __gnat_all_others_value;
#define GNAT_ALL_OTHERS  ((_Unwind_Ptr) &__gnat_all_others_value)

extern const int __gnat_unhandled_others_value;
#define GNAT_UNHANDLED_OTHERS  ((_Unwind_Ptr) &__gnat_unhandled_others_value)

/* Describe the useful region data associated with an unwind context.  */

typedef struct
{
  /* The base pc of the region.  */
  _Unwind_Ptr base;

  /* Pointer to the Language Specific Data for the region.  */
  _Unwind_Ptr lsda;

  /* Call-Site data associated with this region.  */
  unsigned char call_site_encoding;
  const unsigned char *call_site_table;

  /* The base to which are relative landing pad offsets inside the call-site
     entries .  */
  _Unwind_Ptr lp_base;

  /* Action-Table associated with this region.  */
  const unsigned char *action_table;

  /* Ttype data associated with this region.  */
  unsigned char ttype_encoding;
  const unsigned char *ttype_table;
  _Unwind_Ptr ttype_base;

} region_descriptor;

/* Extract and adjust the IP (instruction pointer) from an exception
   context.  */

static _Unwind_Ptr
get_ip_from_context (_Unwind_Context *uw_context)
{
  int ip_before_insn = 0;
#ifdef HAVE_GETIPINFO
  _Unwind_Ptr ip = _Unwind_GetIPInfo (uw_context, &ip_before_insn);
#else
  _Unwind_Ptr ip = _Unwind_GetIP (uw_context);
#endif
  /* Subtract 1 if necessary because GetIPInfo yields a call return address
     in this case, while we are interested in information for the call point.
     This does not always yield the exact call instruction address but always
     brings the IP back within the corresponding region.  */
  if (!ip_before_insn)
    ip--;

  return ip;
}

static void
db_region_for (region_descriptor *region, _Unwind_Ptr ip)
{
#ifndef inhibit_libc
  if (! (db_accepted_codes () & DB_REGIONS))
    return;

  db (DB_REGIONS, "For ip @ %p => ", (void *)ip);

  if (region->lsda)
    db (DB_REGIONS, "lsda @ %p", (void *)region->lsda);
  else
    db (DB_REGIONS, "no lsda");

  db (DB_REGIONS, "\n");
#endif
}

/* Retrieve the ttype entry associated with FILTER in the REGION's
   ttype table.  */

static _Unwind_Ptr
get_ttype_entry_for (region_descriptor *region, long filter)
{
  _Unwind_Ptr ttype_entry;

  filter *= size_of_encoded_value (region->ttype_encoding);
  read_encoded_value_with_base
    (region->ttype_encoding, region->ttype_base,
     region->ttype_table - filter, &ttype_entry);

  return ttype_entry;
}

/* Fill out the REGION descriptor for the provided UW_CONTEXT.  */

static void
get_region_description_for (_Unwind_Context *uw_context,
                            region_descriptor *region)
{
  const unsigned char * p;
  _uleb128_t tmp;
  unsigned char lpbase_encoding;

  /* Get the base address of the lsda information. If the provided context
     is null or if there is no associated language specific data, there's
     nothing we can/should do.  */
  region->lsda
    = (_Unwind_Ptr) (uw_context
		     ? _Unwind_GetLanguageSpecificData (uw_context) : 0);

  if (! region->lsda)
    return;

  /* Parse the lsda and fill the region descriptor.  */
  p = (const unsigned char *)region->lsda;

  region->base = _Unwind_GetRegionStart (uw_context);

  /* Find @LPStart, the base to which landing pad offsets are relative.  */
  lpbase_encoding = *p++;
  if (lpbase_encoding != DW_EH_PE_omit)
    p = read_encoded_value
      (uw_context, lpbase_encoding, p, &region->lp_base);
  else
    region->lp_base = region->base;

  /* Find @TType, the base of the handler and exception spec type data.  */
  region->ttype_encoding = *p++;
  if (region->ttype_encoding != DW_EH_PE_omit)
    {
      p = read_uleb128 (p, &tmp);
      region->ttype_table = p + tmp;
    }
   else
     region->ttype_table = 0;

  region->ttype_base
    = base_of_encoded_value (region->ttype_encoding, uw_context);

  /* Get the encoding and length of the call-site table; the action table
     immediately follows.  */
  region->call_site_encoding = *p++;
  region->call_site_table = read_uleb128 (p, &tmp);

  region->action_table = region->call_site_table + tmp;
}


/* Describe an action to be taken when propagating an exception up to
   some context.  */

enum action_kind
{
  /* Found some call site base data, but need to analyze further
     before being able to decide.  */
  unknown,

  /* There is nothing relevant in the context at hand. */
  nothing,

  /* There are only cleanups to run in this context.  */
  cleanup,

  /* There is a handler for the exception in this context.  */
  handler,

  /* There is a handler for the exception, but it is only for catching
     unhandled exceptions.  */
  unhandler
};

/* filter value for cleanup actions.  */
static const int cleanup_filter = 0;

typedef struct
{
  /* The kind of action to be taken.  */
  enum action_kind kind;

  /* A pointer to the action record entry.  */
  const unsigned char *table_entry;

  /* Where we should jump to actually take an action (trigger a cleanup or an
     exception handler).  */
  _Unwind_Ptr landing_pad;

  /* If we have a handler matching our exception, these are the filter to
     trigger it and the corresponding id.  */
  _Unwind_Sword ttype_filter;

} action_descriptor;

static void
db_action_for (action_descriptor *action, _Unwind_Ptr ip)
{
#ifndef inhibit_libc
  db (DB_ACTIONS, "For ip @ %p => ", (void *)ip);

  switch (action->kind)
     {
     case unknown:
       db (DB_ACTIONS, "lpad @ %p, record @ %p\n",
	   (void *) action->landing_pad, action->table_entry);
       break;

     case nothing:
       db (DB_ACTIONS, "Nothing\n");
       break;

     case cleanup:
       db (DB_ACTIONS, "Cleanup\n");
       break;

     case handler:
       db (DB_ACTIONS, "Handler, filter = %d\n", (int) action->ttype_filter);
       break;

     default:
       db (DB_ACTIONS, "Err? Unexpected action kind !\n");
       break;
    }
#endif
}

/* Search the call_site_table of REGION for an entry appropriate for the
   UW_CONTEXT's IP.  If one is found, store the associated landing_pad
   and action_table entry, and set the ACTION kind to unknown for further
   analysis.  Otherwise, set the ACTION kind to nothing.

   There are two variants of this routine, depending on the underlying
   mechanism (DWARF/SJLJ), which account for differences in the tables.  */

#ifdef __USING_SJLJ_EXCEPTIONS__

#define __builtin_eh_return_data_regno(x) x

static void
get_call_site_action_for (_Unwind_Ptr call_site,
                          region_descriptor *region,
                          action_descriptor *action)
{
  /* call_site is a direct index into the call-site table, with two special
     values : -1 for no-action and 0 for "terminate".  The latter should never
     show up for Ada.  To test for the former, beware that _Unwind_Ptr might
     be unsigned.  */

  if ((int)call_site < 0)
    {
      action->kind = nothing;
    }
  else if (call_site == 0)
    {
      db (DB_ERR, "========> Err, null call_site for Ada/sjlj\n");
      action->kind = nothing;
    }
  else
    {
      _uleb128_t cs_lp, cs_action;
      const unsigned char *p;

      /* Let the caller know there may be an action to take, but let it
	 determine the kind.  */
      action->kind = unknown;

      /* We have a direct index into the call-site table, but this table is
	 made of leb128 values, the encoding length of which is variable.  We
	 can't merely compute an offset from the index, then, but have to read
	 all the entries before the one of interest.  */
      p = region->call_site_table;
      do
	{
	  p = read_uleb128 (p, &cs_lp);
	  p = read_uleb128 (p, &cs_action);
	}
      while (--call_site);

      action->landing_pad = cs_lp + 1;

      if (cs_action)
	action->table_entry = region->action_table + cs_action - 1;
      else
	action->table_entry = 0;
    }
}

#else /* !__USING_SJLJ_EXCEPTIONS__  */

static void
get_call_site_action_for (_Unwind_Ptr ip,
                          region_descriptor *region,
                          action_descriptor *action)
{
  const unsigned char *p = region->call_site_table;

  /* Unless we are able to determine otherwise...  */
  action->kind = nothing;

  db (DB_CSITE, "\n");

  while (p < region->action_table)
    {
      _Unwind_Ptr cs_start, cs_len, cs_lp;
      _uleb128_t cs_action;

      /* Note that all call-site encodings are "absolute" displacements.  */
      p = read_encoded_value (0, region->call_site_encoding, p, &cs_start);
      p = read_encoded_value (0, region->call_site_encoding, p, &cs_len);
      p = read_encoded_value (0, region->call_site_encoding, p, &cs_lp);
      p = read_uleb128 (p, &cs_action);

      db (DB_CSITE,
	  "c_site @ %p (+%p), len = %p, lpad @ %p (+%p)\n",
	  (void *)region->base + cs_start, (void *)cs_start, (void *)cs_len,
	  (void *)region->lp_base + cs_lp, (void *)cs_lp);

      /* The table is sorted, so if we've passed the IP, stop.  */
      if (ip < region->base + cs_start)
 	break;

      /* If we have a match, fill the ACTION fields accordingly.  */
      else if (ip < region->base + cs_start + cs_len)
	{
	  /* Let the caller know there may be an action to take, but let it
	     determine the kind.  */
	  action->kind = unknown;

	  if (cs_lp)
	    action->landing_pad = region->lp_base + cs_lp;
	  else
	    action->landing_pad = 0;

	  if (cs_action)
	    action->table_entry = region->action_table + cs_action - 1;
	  else
	    action->table_entry = 0;

	  db (DB_CSITE, "+++\n");
	  return;
	}
    }

  db (DB_CSITE, "---\n");
}

#endif /* __USING_SJLJ_EXCEPTIONS__  */

/* With CHOICE an exception choice representing an "exception - when"
   argument, and PROPAGATED_EXCEPTION a pointer to the currently propagated
   occurrence, return true if the latter matches the former, that is, if
   PROPAGATED_EXCEPTION is caught by the handling code controlled by CHOICE.
   This takes care of the special Non_Ada_Error case on VMS.  */

#define Is_Handled_By_Others  __gnat_is_handled_by_others
#define Language_For          __gnat_language_for
#define Import_Code_For       __gnat_import_code_for
#define EID_For               __gnat_eid_for

extern bool Is_Handled_By_Others (_Unwind_Ptr eid);
extern char Language_For (_Unwind_Ptr eid);

extern Exception_Code Import_Code_For (_Unwind_Ptr eid);

extern Exception_Id EID_For (_GNAT_Exception * e);

static enum action_kind
is_handled_by (_Unwind_Ptr choice, _GNAT_Exception * propagated_exception)
{
  if (choice == GNAT_ALL_OTHERS)
    return handler;

  if (propagated_exception->common.exception_class == GNAT_EXCEPTION_CLASS)
    {
      /* Pointer to the GNAT exception data corresponding to the propagated
         occurrence.  */
      _Unwind_Ptr E = (_Unwind_Ptr) EID_For (propagated_exception);

      if (choice == GNAT_UNHANDLED_OTHERS)
	return unhandler;

      E = (_Unwind_Ptr) EID_For (propagated_exception);

      /* Base matching rules: An exception data (id) matches itself, "when
         all_others" matches anything and "when others" matches anything
         unless explicitly stated otherwise in the propagated occurrence.  */
      if (choice == E || (choice == GNAT_OTHERS && Is_Handled_By_Others (E)))
	return handler;

      /* In addition, on OpenVMS, Non_Ada_Error matches VMS exceptions, and we
         may have different exception data pointers that should match for the
         same condition code, if both an export and an import have been
         registered.  The import code for both the choice and the propagated
         occurrence are expected to have been masked off regarding severity
         bits already (at registration time for the former and from within the
         low level exception vector for the latter).  */
#ifdef VMS
#     define Non_Ada_Error system__aux_dec__non_ada_error
      extern struct Exception_Data Non_Ada_Error;

      if ((Language_For (E) == 'V'
	   && choice != GNAT_OTHERS
	   && ((Language_For (choice) == 'V'
		&& Import_Code_For (choice) != 0
		&& Import_Code_For (choice) == Import_Code_For (E))
	       || choice == (_Unwind_Ptr)&Non_Ada_Error)))
	return handler;
#endif
    }
  else
    {
#     define Foreign_Exception system__exceptions__foreign_exception
      extern struct Exception_Data Foreign_Exception;

      if (choice == GNAT_ALL_OTHERS
	  || choice == GNAT_OTHERS
	  || choice == (_Unwind_Ptr) &Foreign_Exception)
	return handler;
    }
  return nothing;
}

/* Fill out the ACTION to be taken from propagating UW_EXCEPTION up to
   UW_CONTEXT in REGION.  */

static void
get_action_description_for (_Unwind_Ptr ip,
                            _Unwind_Exception *uw_exception,
                            _Unwind_Action uw_phase,
                            region_descriptor *region,
                            action_descriptor *action)
{
  _GNAT_Exception *gnat_exception = (_GNAT_Exception *) uw_exception;

  /* Search the call site table first, which may get us a landing pad as well
     as the head of an action record list.  */
  get_call_site_action_for (ip, region, action);
  db_action_for (action, ip);

  /* If there is not even a call_site entry, we are done.  */
  if (action->kind == nothing)
    return;

  /* Otherwise, check what we have at the place of the call site.  */

  /* No landing pad => no cleanups or handlers.  */
  if (action->landing_pad == 0)
    {
      action->kind = nothing;
      return;
    }

  /* Landing pad + null table entry => only cleanups.  */
  else if (action->table_entry == 0)
    {
      action->kind = cleanup;
      action->ttype_filter = cleanup_filter;
      /* The filter initialization is not strictly necessary, as cleanup-only
	 landing pads don't look at the filter value.  It is there to ensure
	 we don't pass random values and so trigger potential confusion when
	 installing the context later on.  */
      return;
    }

  /* Landing pad + Table entry => handlers + possible cleanups.  */
  else
    {
      const unsigned char * p = action->table_entry;

      _sleb128_t ar_filter, ar_disp;

      action->kind = nothing;

      while (1)
	{
	  p = read_sleb128 (p, &ar_filter);
	  read_sleb128 (p, &ar_disp);
	  /* Don't assign p here, as it will be incremented by ar_disp
	     below.  */

	  /* Null filters are for cleanups. */
	  if (ar_filter == cleanup_filter)
	    {
	      action->kind = cleanup;
	      action->ttype_filter = cleanup_filter;
	      /* The filter initialization is required here, to ensure
		 the target landing pad branches to the cleanup code if
		 we happen not to find a matching handler.  */
	    }

	  /* Positive filters are for regular handlers.  */
	  else if (ar_filter > 0)
	    {
              /* Do not catch an exception if the _UA_FORCE_UNWIND flag is
                 passed (to follow the ABI).  */
              if (!(uw_phase & _UA_FORCE_UNWIND))
                {
		  enum action_kind act;

                  /* See if the filter we have is for an exception which
                     matches the one we are propagating.  */
                  _Unwind_Ptr choice = get_ttype_entry_for (region, ar_filter);

		  act = is_handled_by (choice, gnat_exception);
                  if (act != nothing)
                    {
		      action->kind = act;
                      action->ttype_filter = ar_filter;
                      return;
                    }
                }
	    }

	  /* Negative filter values are for C++ exception specifications.
	     Should not be there for Ada :/  */
	  else
	    db (DB_ERR, "========> Err, filter < 0 for Ada/dwarf\n");

	  if (ar_disp == 0)
	    return;

	  p += ar_disp;
	}
    }
}

/* Setup in UW_CONTEXT the eh return target IP and data registers, which will
   be restored with the others and retrieved by the landing pad once the jump
   occurred.  */

static void
setup_to_install (_Unwind_Context *uw_context,
                  _Unwind_Exception *uw_exception,
                  _Unwind_Ptr uw_landing_pad,
                  int uw_filter)
{
  /* 1/ exception object pointer, which might be provided back to
     _Unwind_Resume (and thus to this personality routine) if we are jumping
     to a cleanup.  */
  _Unwind_SetGR (uw_context, __builtin_eh_return_data_regno (0),
		 (_Unwind_Word)uw_exception);

  /* 2/ handler switch value register, which will also be used by the target
     landing pad to decide what action it shall take.  */
  _Unwind_SetGR (uw_context, __builtin_eh_return_data_regno (1),
		 (_Unwind_Word)uw_filter);

  /* Setup the address we should jump at to reach the code where there is the
     "something" we found.  */
  _Unwind_SetIP (uw_context, uw_landing_pad);
}

/* The following is defined from a-except.adb. Its purpose is to enable
   automatic backtraces upon exception raise, as provided through the
   GNAT.Traceback facilities.  */
extern void __gnat_notify_handled_exception (struct Exception_Occurrence *);
extern void __gnat_notify_unhandled_exception (struct Exception_Occurrence *);

/* Below is the eh personality routine per se. We currently assume that only
   GNU-Ada exceptions are met.  */

#ifdef __USING_SJLJ_EXCEPTIONS__
#define PERSONALITY_FUNCTION    __gnat_personality_sj0
#elif defined(__SEH__)
#define PERSONALITY_FUNCTION    __gnat_personality_imp
#else
#define PERSONALITY_FUNCTION    __gnat_personality_v0
#endif

/* Major tweak for ia64-vms : the CHF propagation phase calls this personality
   routine with sigargs/mechargs arguments and has very specific expectations
   on possible return values.

   We handle this with a number of specific tricks:

   1. We tweak the personality routine prototype to have the "version" and
      "phases" two first arguments be void * instead of int and _Unwind_Action
      as nominally expected in the GCC context.

      This allows us to access the full range of bits passed in every case and
      has no impact on the callers side since each argument remains assigned
      the same single 64bit slot.

   2. We retrieve the corresponding int and _Unwind_Action values within the
      routine for regular use with truncating conversions. This is a noop when
      called from the libgcc unwinder.

   3. We assume we're called by the VMS CHF when unexpected bits are set in
      both those values. The incoming arguments are then real sigargs and
      mechargs pointers, which we then redirect to __gnat_handle_vms_condition
      for proper processing.
*/
#if defined (VMS) && defined (__IA64)
typedef void * version_arg_t;
typedef void * phases_arg_t;
#else
typedef int version_arg_t;
typedef _Unwind_Action phases_arg_t;
#endif

#ifdef __SEH__
static
#endif
_Unwind_Reason_Code
PERSONALITY_FUNCTION (version_arg_t, phases_arg_t,
                      _Unwind_Exception_Class, _Unwind_Exception *,
                      _Unwind_Context *);

_Unwind_Reason_Code
PERSONALITY_FUNCTION (version_arg_t version_arg,
                      phases_arg_t phases_arg,
                      _Unwind_Exception_Class uw_exception_class
		         ATTRIBUTE_UNUSED,
                      _Unwind_Exception *uw_exception,
                      _Unwind_Context *uw_context)
{
  /* Fetch the version and phases args with their nominal ABI types for later
     use. This is a noop everywhere except on ia64-vms when called from the
     Condition Handling Facility.  */
  int uw_version = (int) version_arg;
  _Unwind_Action uw_phases = (_Unwind_Action) phases_arg;
  region_descriptor region;
  action_descriptor action;
  _Unwind_Ptr ip;

  /* Check that we're called from the ABI context we expect, with a major
     possible variation on VMS for IA64.  */
  if (uw_version != 1)
    {
#if defined (VMS) && defined (__IA64)

      /* Assume we're called with sigargs/mechargs arguments if really
	 unexpected bits are set in our first two formals.  Redirect to the
	 GNAT condition handling code in this case.  */

      extern long __gnat_handle_vms_condition (void *, void *);

      unsigned int version_unexpected_bits_mask = 0xffffff00U;
      unsigned int phases_unexpected_bits_mask  = 0xffffff00U;

      if ((unsigned int)uw_version & version_unexpected_bits_mask
	  && (unsigned int)uw_phases & phases_unexpected_bits_mask)
	return __gnat_handle_vms_condition (version_arg, phases_arg);
#endif

      return _URC_FATAL_PHASE1_ERROR;
    }

  db_indent (DB_INDENT_RESET);
  db_phases (uw_phases);
  db_indent (DB_INDENT_INCREASE);

  /* Get the region description for the context we were provided with. This
     will tell us if there is some lsda, call_site, action and/or ttype data
     for the associated ip.  */
  get_region_description_for (uw_context, &region);
  ip = get_ip_from_context (uw_context);
  db_region_for (&region, ip);

  /* No LSDA => no handlers or cleanups => we shall unwind further up.  */
  if (! region.lsda)
    return _URC_CONTINUE_UNWIND;

  /* Search the call-site and action-record tables for the action associated
     with this IP.  */
  get_action_description_for (ip, uw_exception, uw_phases, &region, &action);
  db_action_for (&action, ip);

  /* Whatever the phase, if there is nothing relevant in this frame,
     unwinding should just go on.  */
  if (action.kind == nothing)
    return _URC_CONTINUE_UNWIND;

  /* If we found something in search phase, we should return a code indicating
     what to do next depending on what we found. If we only have cleanups
     around, we shall try to unwind further up to find a handler, otherwise,
     tell we have a handler, which will trigger the second phase.  */
  if (uw_phases & _UA_SEARCH_PHASE)
    {
      if (action.kind == cleanup)
	{
	  return _URC_CONTINUE_UNWIND;
	}
      else
	{
	  struct Exception_Occurrence *excep;

	  /* Trigger the appropriate notification routines before the second
	     phase starts, which ensures the stack is still intact.
             First, setup the Ada occurrence.  */
          excep = __gnat_setup_current_excep (uw_exception);
	  if (action.kind == unhandler)
	    __gnat_notify_unhandled_exception (excep);
	  else
	    __gnat_notify_handled_exception (excep);

	  return _URC_HANDLER_FOUND;
	}
    }

  /* We found something in cleanup/handler phase, which might be the handler
     or a cleanup for a handled occurrence, or a cleanup for an unhandled
     occurrence (we are in a FORCED_UNWIND phase in this case). Install the
     context to get there.  */

  setup_to_install
    (uw_context, uw_exception, action.landing_pad, action.ttype_filter);

  /* Write current exception, so that it can be retrieved from Ada.  */
  __gnat_setup_current_excep (uw_exception);

  return _URC_INSTALL_CONTEXT;
}

/* Callback routine called by Unwind_ForcedUnwind to execute all the cleanup
   before exiting the task.  */

_Unwind_Reason_Code
__gnat_cleanupunwind_handler (int version ATTRIBUTE_UNUSED,
			      _Unwind_Action phases,
			      _Unwind_Exception_Class eclass ATTRIBUTE_UNUSED,
			      struct _Unwind_Exception *exception,
			      struct _Unwind_Context *context ATTRIBUTE_UNUSED,
			      void *arg ATTRIBUTE_UNUSED)
{
  /* Terminate when the end of the stack is reached.  */
  if ((phases & _UA_END_OF_STACK) != 0
#if defined (__ia64__) && defined (__hpux__) && defined (USE_LIBUNWIND_EXCEPTIONS)
      /* Strictely follow the ia64 ABI: when end of stack is reached,
	 the callback will be called with a NULL stack pointer.
	 No need for that when using libgcc unwinder.  */
      || _Unwind_GetGR (context, 12) == 0
#endif
      )
    __gnat_unhandled_except_handler (exception);

  /* We know there is at least one cleanup further up. Return so that it
     is searched and entered, after which Unwind_Resume will be called
     and this hook will gain control again.  */
  return _URC_NO_REASON;
}

/* Define the consistently named wrappers imported by Propagate_Exception.  */

_Unwind_Reason_Code
__gnat_Unwind_RaiseException (_Unwind_Exception *e)
{
#ifdef __USING_SJLJ_EXCEPTIONS__
  return _Unwind_SjLj_RaiseException (e);
#else
  return _Unwind_RaiseException (e);
#endif
}

_Unwind_Reason_Code
__gnat_Unwind_ForcedUnwind (_Unwind_Exception *e,
			    void *handler,
			    void *argument)
{
#ifdef __USING_SJLJ_EXCEPTIONS__
  return _Unwind_SjLj_ForcedUnwind (e, handler, argument);
#else
  return _Unwind_ForcedUnwind (e, handler, argument);
#endif
}

#ifdef __SEH__

#define STATUS_USER_DEFINED		(1U << 29)

/* From unwind-seh.c.  */
#define GCC_MAGIC			(('G' << 16) | ('C' << 8) | 'C')
#define GCC_EXCEPTION(TYPE)		\
       (STATUS_USER_DEFINED | ((TYPE) << 24) | GCC_MAGIC)
#define STATUS_GCC_THROW		GCC_EXCEPTION (0)

EXCEPTION_DISPOSITION __gnat_SEH_error_handler
 (struct _EXCEPTION_RECORD*, void*, struct _CONTEXT*, void*);

struct Exception_Data *
__gnat_map_SEH (EXCEPTION_RECORD* ExceptionRecord, const char **msg);

struct _Unwind_Exception *
__gnat_create_machine_occurrence_from_signal_handler (Exception_Id,
						      const char *);

/* Unwind opcodes.  */
#define UWOP_PUSH_NONVOL 0
#define UWOP_ALLOC_LARGE 1
#define UWOP_ALLOC_SMALL 2
#define UWOP_SET_FPREG	 3
#define UWOP_SAVE_NONVOL 4
#define UWOP_SAVE_NONVOL_FAR 5
#define UWOP_SAVE_XMM128 8
#define UWOP_SAVE_XMM128_FAR 9
#define UWOP_PUSH_MACHFRAME 10

/* Modify the IP value saved in the machine frame.  This is really a kludge,
   that will be removed if we could propagate the Windows exception (and not
   the GCC one).
   What is very wrong is that the Windows unwinder will try to decode the
   instruction at IP, which isn't valid anymore after the adjust.  */

static void
__gnat_adjust_context (unsigned char *unw, ULONG64 rsp)
{
  unsigned int len;

  /* Version = 1, no flags, no prolog.  */
  if (unw[0] != 1 || unw[1] != 0)
    return;
  len = unw[2];
  /* No frame pointer.  */
  if (unw[3] != 0)
    return;
  unw += 4;
  while (len > 0)
    {
      /* Offset in prolog = 0.  */
      if (unw[0] != 0)
	return;
      switch (unw[1] & 0xf)
	{
	case UWOP_ALLOC_LARGE:
	  /* Expect < 512KB.  */
	  if ((unw[1] & 0xf0) != 0)
	    return;
	  rsp += *(unsigned short *)(unw + 2) * 8;
	  len--;
	  unw += 2;
	  break;
	case UWOP_SAVE_NONVOL:
	case UWOP_SAVE_XMM128:
	  len--;
	  unw += 2;
	  break;
	case UWOP_PUSH_MACHFRAME:
	  {
	    ULONG64 *rip;
	    rip = (ULONG64 *)rsp;
	    if ((unw[1] & 0xf0) == 0x10)
	      rip++;
	    /* Adjust rip.  */
	    (*rip)++;
	  }
	  return;
	default:
	  /* Unexpected.  */
	  return;
	}
      unw += 2;
      len--;
    }
}

EXCEPTION_DISPOSITION
__gnat_personality_seh0 (PEXCEPTION_RECORD ms_exc, void *this_frame,
			 PCONTEXT ms_orig_context,
			 PDISPATCHER_CONTEXT ms_disp)
{
  /* Possibly transform run-time errors into Ada exceptions.  As a small
     optimization, we call __gnat_SEH_error_handler only on non-user
     exceptions.  */
  if (!(ms_exc->ExceptionCode & STATUS_USER_DEFINED))
    {
      struct Exception_Data *exception;
      const char *msg;
      ULONG64 excpip = (ULONG64) ms_exc->ExceptionAddress;

      if (excpip != 0
	  && excpip >= (ms_disp->ImageBase
			+ ms_disp->FunctionEntry->BeginAddress)
	  && excpip < (ms_disp->ImageBase
		       + ms_disp->FunctionEntry->EndAddress))
	{
	  /* This is a fault in this function.  We need to adjust the return
	     address before raising the GCC exception.  */
	  CONTEXT context;
	  PRUNTIME_FUNCTION mf_func = NULL;
	  ULONG64 mf_imagebase;
	  ULONG64 mf_rsp = 0;

	  /* Get the context.  */
	  RtlCaptureContext (&context);

	  while (1)
	    {
	      PRUNTIME_FUNCTION RuntimeFunction;
	      ULONG64 ImageBase;
	      VOID *HandlerData;
	      ULONG64 EstablisherFrame;

	      /* Get function metadata.  */
	      RuntimeFunction = RtlLookupFunctionEntry
		(context.Rip, &ImageBase, ms_disp->HistoryTable);
	      if (RuntimeFunction == ms_disp->FunctionEntry)
		break;
	      mf_func = RuntimeFunction;
	      mf_imagebase = ImageBase;
	      mf_rsp = context.Rsp;

	      if (!RuntimeFunction)
		{
		  /* In case of failure, assume this is a leaf function.  */
		  context.Rip = *(ULONG64 *) context.Rsp;
		  context.Rsp += 8;
		}
	      else
		{
		  /* Unwind.  */
		  RtlVirtualUnwind (0, ImageBase, context.Rip, RuntimeFunction,
				    &context, &HandlerData, &EstablisherFrame,
				    NULL);
		}

	      /* 0 means bottom of the stack.  */
	      if (context.Rip == 0)
		{
		  mf_func = NULL;
		  break;
		}
	    }
	  if (mf_func != NULL)
	    __gnat_adjust_context
	      ((unsigned char *)(mf_imagebase + mf_func->UnwindData), mf_rsp);
	}

      exception = __gnat_map_SEH (ms_exc, &msg);
      if (exception != NULL)
	{
	  struct _Unwind_Exception *exc;

	  /* Directly convert the system exception to a GCC one.
	     This is really breaking the API, but is necessary for stack size
	     reasons: the normal way is to call Raise_From_Signal_Handler,
	     which build the exception and calls _Unwind_RaiseException, which
	     unwinds the stack and will call this personality routine. But
	     the Windows unwinder needs about 2KB of stack.  */
	  exc = __gnat_create_machine_occurrence_from_signal_handler
	    (exception, msg);
	  memset (exc->private_, 0, sizeof (exc->private_));
	  ms_exc->ExceptionCode = STATUS_GCC_THROW;
	  ms_exc->NumberParameters = 1;
	  ms_exc->ExceptionInformation[0] = (ULONG_PTR)exc;
	}

    }

  return _GCC_specific_handler (ms_exc, this_frame, ms_orig_context,
				ms_disp, __gnat_personality_imp);
}
#endif /* SEH */
