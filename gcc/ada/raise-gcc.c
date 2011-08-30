/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                            R A I S E - G C C                             *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *             Copyright (C) 1992-2011, Free Software Foundation, Inc.      *
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

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
#include <sys/stat.h>
#include <stdarg.h>
typedef char bool;
# define true 1
# define false 0
#else
#include "config.h"
#include "system.h"
#endif

#include "adaint.h"
#include "raise.h"

#ifdef __APPLE__
/* On MacOS X, versions older than 10.5 don't export _Unwind_GetIPInfo.  */
#undef HAVE_GETIPINFO
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 1050
#define HAVE_GETIPINFO 1
#endif
#endif

/* The names of a couple of "standard" routines for unwinding/propagation
   actually vary depending on the underlying GCC scheme for exception handling
   (SJLJ or DWARF). We need a consistently named interface to import from
   a-except, so wrappers are defined here.

   Besides, even though the compiler is never setup to use the GCC propagation
   circuitry, it still relies on exceptions internally and part of the sources
   to handle to exceptions are shared with the run-time library.  We need
   dummy definitions for the wrappers to satisfy the linker in this case.

   The types to be used by those wrappers in the run-time library are target
   types exported by unwind.h.  We used to piggyback on them for the compiler
   stubs, but there is no guarantee that unwind.h is always in sight so we
   define our own set below.  These are dummy types as the wrappers are never
   called in the compiler case.  */

#ifdef IN_RTS

#include "unwind.h"

typedef struct _Unwind_Context _Unwind_Context;
typedef struct _Unwind_Exception _Unwind_Exception;

#else

typedef void _Unwind_Context;
typedef void _Unwind_Exception;
typedef int  _Unwind_Reason_Code;

#endif

_Unwind_Reason_Code
__gnat_Unwind_RaiseException (_Unwind_Exception *);

_Unwind_Reason_Code
__gnat_Unwind_ForcedUnwind (_Unwind_Exception *, void *, void *);

extern void __gnat_setup_current_excep (_Unwind_Exception *);

#ifdef IN_RTS   /* For eh personality routine */

#include "dwarf2.h"
#include "unwind-dw2-fde.h"
#include "unwind-pe.h"

/* The known and handled exception classes.  */

#define CXX_EXCEPTION_CLASS 0x474e5543432b2b00ULL
#define GNAT_EXCEPTION_CLASS 0x474e552d41646100ULL

/* --------------------------------------------------------------
   -- The DB stuff below is there for debugging purposes only. --
   -------------------------------------------------------------- */

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
    {
      current_indentation_level = 0;
    }

  if (requests & DB_INDENT_INCREASE)
    {
      current_indentation_level ++;
    }

  if (requests & DB_INDENT_DECREASE)
    {
      current_indentation_level --;
    }

  if (requests & DB_INDENT_NEWLINE)
    {
      fprintf (stderr, "\n");
    }

  if (requests & DB_INDENT_OUTPUT)
    {
      fprintf (stderr, "%*s",
	       current_indentation_level * DB_INDENT_UNIT, " ");
    }

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


/* ---------------------------------------------------------------
   --  Now come a set of useful structures and helper routines. --
   --------------------------------------------------------------- */

/* There are three major runtime tables involved, generated by the
   GCC back-end. Contents slightly vary depending on the underlying
   implementation scheme (dwarf zero cost / sjlj).

   =======================================
   * Tables for the dwarf zero cost case *
   =======================================

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
   There is at most one action list per call-site entry.

   A null action-filter indicates a cleanup.

   Non null action-filters provide an index into the ttypes [] table
   (see below), from which information may be retrieved to check if it
   matches the exception being propagated.

   action-filter > 0  means there is a regular handler to be run,

   action-filter < 0  means there is a some "exception_specification"
                      data to retrieve, which is only relevant for C++
		      and should never show up for Ada.

   next-action indexes the next entry in the list. 0 indicates there is
   no other entry.

   ttypes []
   ---------------
   * ttype-value *
   ---------------

   A null value indicates a catch-all handler in C++, and an "others"
   handler in Ada.

   Non null values are used to match the exception being propagated:
   In C++ this is a pointer to some rtti data, while in Ada this is an
   exception id.

   The special id value 1 indicates an "all_others" handler.

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
    |            |  0 => "others"      |     |
    |    ...     |  1 => "all others"  | <---+
    |            |  X => exception id  |
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
	   +--> get_region_descriptor_for (context)
	   |
	   +--> get_action_descriptor_for (context, exception, region)
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
db_region_for (region_descriptor *region, _Unwind_Context *uw_context)
{
  _Unwind_Ptr ip;

  if (! (db_accepted_codes () & DB_REGIONS))
    return;

  ip = get_ip_from_context (uw_context);

  db (DB_REGIONS, "For ip @ 0x%08x => ", ip);

  if (region->lsda)
    db (DB_REGIONS, "lsda @ 0x%x", region->lsda);
  else
    db (DB_REGIONS, "no lsda");

  db (DB_REGIONS, "\n");
}

/* Retrieve the ttype entry associated with FILTER in the REGION's
   ttype table.  */

static const _Unwind_Ptr
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
  p = (char *)region->lsda;

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

typedef enum
{
  /* Found some call site base data, but need to analyze further
     before being able to decide.  */
  unknown,

  /* There is nothing relevant in the context at hand. */
  nothing,

  /* There are only cleanups to run in this context.  */
  cleanup,

  /* There is a handler for the exception in this context.  */
  handler
} action_kind;

/* filter value for cleanup actions.  */
static const int cleanup_filter = 0;

typedef struct
{
  /* The kind of action to be taken.  */
  action_kind kind;

  /* A pointer to the action record entry.  */
  const unsigned char *table_entry;

  /* Where we should jump to actually take an action (trigger a cleanup or an
     exception handler).  */
  _Unwind_Ptr landing_pad;

  /* If we have a handler matching our exception, these are the filter to
     trigger it and the corresponding id.  */
  _Unwind_Sword ttype_filter;
  _Unwind_Ptr   ttype_entry;

} action_descriptor;

static void
db_action_for (action_descriptor *action, _Unwind_Context *uw_context)
{
  _Unwind_Ptr ip = get_ip_from_context (uw_context);

  db (DB_ACTIONS, "For ip @ 0x%08x => ", ip);

  switch (action->kind)
     {
     case unknown:
       db (DB_ACTIONS, "lpad @ 0x%x, record @ 0x%x\n",
	   action->landing_pad, action->table_entry);
       break;

     case nothing:
       db (DB_ACTIONS, "Nothing\n");
       break;

     case cleanup:
       db (DB_ACTIONS, "Cleanup\n");
       break;

     case handler:
       db (DB_ACTIONS, "Handler, filter = %d\n", action->ttype_filter);
       break;

     default:
       db (DB_ACTIONS, "Err? Unexpected action kind !\n");
       break;
    }

  return;
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
get_call_site_action_for (_Unwind_Context *uw_context,
                          region_descriptor *region,
                          action_descriptor *action)
{
  _Unwind_Ptr call_site = get_ip_from_context (uw_context);

  /* call_site is a direct index into the call-site table, with two special
     values : -1 for no-action and 0 for "terminate".  The latter should never
     show up for Ada.  To test for the former, beware that _Unwind_Ptr might
     be unsigned.  */

  if ((int)call_site < 0)
    {
      action->kind = nothing;
      return;
    }
  else if (call_site == 0)
    {
      db (DB_ERR, "========> Err, null call_site for Ada/sjlj\n");
      action->kind = nothing;
      return;
    }
  else
    {
      _uleb128_t cs_lp, cs_action;

      /* Let the caller know there may be an action to take, but let it
	 determine the kind.  */
      action->kind = unknown;

      /* We have a direct index into the call-site table, but this table is
	 made of leb128 values, the encoding length of which is variable.  We
	 can't merely compute an offset from the index, then, but have to read
	 all the entries before the one of interest.  */

      const unsigned char *p = region->call_site_table;

      do {
	p = read_uleb128 (p, &cs_lp);
	p = read_uleb128 (p, &cs_action);
      } while (--call_site);

      action->landing_pad = cs_lp + 1;

      if (cs_action)
	action->table_entry = region->action_table + cs_action - 1;
      else
	action->table_entry = 0;

      return;
    }
}

#else /* !__USING_SJLJ_EXCEPTIONS__  */

static void
get_call_site_action_for (_Unwind_Context *uw_context,
                          region_descriptor *region,
                          action_descriptor *action)
{
  const unsigned char *p = region->call_site_table;
  _Unwind_Ptr ip = get_ip_from_context (uw_context);

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
	  "c_site @ 0x%08x (+0x%03x), len = %3d, lpad @ 0x%08x (+0x%03x)\n",
	  region->base+cs_start, cs_start, cs_len,
	  region->lp_base+cs_lp, cs_lp);

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

static int
is_handled_by (_Unwind_Ptr choice, _GNAT_Exception * propagated_exception)
{
  if (propagated_exception->common.exception_class == GNAT_EXCEPTION_CLASS)
    {
      /* Pointer to the GNAT exception data corresponding to the propagated
         occurrence.  */
      _Unwind_Ptr E = (_Unwind_Ptr) EID_For (propagated_exception);

      /* Base matching rules: An exception data (id) matches itself, "when
         all_others" matches anything and "when others" matches anything
         unless explicitly stated otherwise in the propagated occurrence.  */

      bool is_handled =
        choice == E
        || choice == GNAT_ALL_OTHERS
        || (choice == GNAT_OTHERS && Is_Handled_By_Others (E));

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

      is_handled |=
        (Language_For (E) == 'V'
         && choice != GNAT_OTHERS && choice != GNAT_ALL_OTHERS
         && ((Language_For (choice) == 'V' && Import_Code_For (choice) != 0
              && Import_Code_For (choice) == Import_Code_For (E))
             || choice == (_Unwind_Ptr)&Non_Ada_Error));
#endif

      return is_handled;
    }
  else
    {
#     define Foreign_Exception system__exceptions__foreign_exception;
      extern struct Exception_Data Foreign_Exception;

      return choice == GNAT_ALL_OTHERS
        || choice == GNAT_OTHERS
        || choice == (_Unwind_Ptr)&Foreign_Exception;
    }
}

/* Fill out the ACTION to be taken from propagating UW_EXCEPTION up to
   UW_CONTEXT in REGION.  */

static void
get_action_description_for (_Unwind_Context *uw_context,
                            _Unwind_Exception *uw_exception,
                            _Unwind_Action uw_phase,
                            region_descriptor *region,
                            action_descriptor *action)
{
  _GNAT_Exception * gnat_exception = (_GNAT_Exception *) uw_exception;

  /* Search the call site table first, which may get us a landing pad as well
     as the head of an action record list.  */
  get_call_site_action_for (uw_context, region, action);
  db_action_for (action, uw_context);

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
                  /* See if the filter we have is for an exception which
                     matches the one we are propagating.  */
                  _Unwind_Ptr choice = get_ttype_entry_for (region, ar_filter);

                  if (is_handled_by (choice, gnat_exception))
                    {
                      action->kind = handler;
                      action->ttype_filter = ar_filter;
                      action->ttype_entry = choice;
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
extern void __gnat_notify_handled_exception (void);
extern void __gnat_notify_unhandled_exception (void);

/* Below is the eh personality routine per se. We currently assume that only
   GNU-Ada exceptions are met.  */

#ifdef __USING_SJLJ_EXCEPTIONS__
#define PERSONALITY_FUNCTION    __gnat_personality_sj0
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

_Unwind_Reason_Code
PERSONALITY_FUNCTION (version_arg_t, phases_arg_t,
                      _Unwind_Exception_Class, _Unwind_Exception *,
                      _Unwind_Context *);

_Unwind_Reason_Code
PERSONALITY_FUNCTION (version_arg_t version_arg,
                      phases_arg_t phases_arg,
                      _Unwind_Exception_Class uw_exception_class,
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
  db_region_for (&region, uw_context);

  /* No LSDA => no handlers or cleanups => we shall unwind further up.  */
  if (! region.lsda)
    return _URC_CONTINUE_UNWIND;

  /* Search the call-site and action-record tables for the action associated
     with this IP.  */
  get_action_description_for (uw_context, uw_exception, uw_phases,
                              &region, &action);
  db_action_for (&action, uw_context);

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
	  /* Trigger the appropriate notification routines before the second
	     phase starts, which ensures the stack is still intact.
             First, setup the Ada occurrence.  */
          __gnat_setup_current_excep (uw_exception);
	  __gnat_notify_handled_exception ();

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

/* Define the consistently named wrappers imported by Propagate_Exception.  */

#ifdef __USING_SJLJ_EXCEPTIONS__

#undef _Unwind_RaiseException

_Unwind_Reason_Code
__gnat_Unwind_RaiseException (_Unwind_Exception *e)
{
  return _Unwind_SjLj_RaiseException (e);
}


#undef _Unwind_ForcedUnwind

_Unwind_Reason_Code
__gnat_Unwind_ForcedUnwind (_Unwind_Exception *e,
                            void * handler,
                            void * argument)
{
  return _Unwind_SjLj_ForcedUnwind (e, handler, argument);
}


#else /* __USING_SJLJ_EXCEPTIONS__ */

_Unwind_Reason_Code
__gnat_Unwind_RaiseException (_Unwind_Exception *e)
{
  return _Unwind_RaiseException (e);
}

_Unwind_Reason_Code
__gnat_Unwind_ForcedUnwind (_Unwind_Exception *e,
                            void * handler,
                            void * argument)
{
  return _Unwind_ForcedUnwind (e, handler, argument);
}

#endif /* __USING_SJLJ_EXCEPTIONS__ */

#else
/* ! IN_RTS  */

/* Define the corresponding stubs for the compiler.  */

/* We don't want fancy_abort here.  */
#undef abort

_Unwind_Reason_Code
__gnat_Unwind_RaiseException (_Unwind_Exception *e ATTRIBUTE_UNUSED)
{
  abort ();
}


_Unwind_Reason_Code
__gnat_Unwind_ForcedUnwind (_Unwind_Exception *e ATTRIBUTE_UNUSED,
                            void * handler ATTRIBUTE_UNUSED,
                            void * argument ATTRIBUTE_UNUSED)
{
  abort ();
}

#endif /* IN_RTS */
