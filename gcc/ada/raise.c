/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                R A I S E                                 *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                                                                          *
 *             Copyright (C) 1992-2002, Free Software Foundation, Inc.      *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* Routines to support runtime exception handling */

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
#include <sys/stat.h>
typedef char bool;
# define true 1
# define false 0
#else
#include "config.h"
#include "system.h"
#endif

#include "adaint.h"
#include "raise.h"

/*  We have not yet figured out how to import this directly */

void
_gnat_builtin_longjmp (ptr, flag)
     void *ptr;
     int flag ATTRIBUTE_UNUSED;
{
   __builtin_longjmp (ptr, 1);
}

/* When an exception is raised for which no handler exists, the procedure
   Ada.Exceptions.Unhandled_Exception is called, which performs the call to
   adafinal to complete finalization, and then prints out the error messages
   for the unhandled exception. The final step is to call this routine, which
   performs any system dependent cleanup required.  */

void
__gnat_unhandled_terminate ()
{
  /* Special termination handling for VMS */

#ifdef VMS
    {
      long prvhnd;

      /* Remove the exception vector so it won't intercept any errors
	 in the call to exit, and go into and endless loop */

      SYS$SETEXV (1, 0, 3, &prvhnd);
      __gnat_os_exit (1);
    }

/* Termination handling for all other systems. */

#elif !defined (__RT__)
    __gnat_os_exit (1);
#endif
}

/* Below is the code related to the integration of the GCC mechanism for
   exception handling. Still work in progress. */

#include "unwind.h"

/* If the underlying GCC scheme for exception handling is SJLJ, the standard
   propagation routine (_Unwind_RaiseException) is actually renamed using a
   #define directive (see unwing-sjlj.c). We need a consistently named
   interface to import from a-except, so stubs are defined here, at the end
   of this file.  */

_Unwind_Reason_Code
__gnat_Unwind_RaiseException PARAMS ((struct _Unwind_Exception *));


/* Exception Handling personality routine for Ada.

   ??? It is currently inspired from the one for C++, needs cleanups and
   additional comments. It also contains a big bunch of debugging code that
   we shall get rid of at some point.  */

#ifdef IN_RTS   /* For eh personality routine */

/* ??? Does it make any sense to leave this for the compiler ?   */

#include "dwarf2.h"
#include "unwind-dw2-fde.h"
#include "unwind-pe.h"

/* First define a set of useful structures and helper routines.  */

typedef struct _Unwind_Context _Unwind_Context;

struct lsda_header_info
{
  _Unwind_Ptr Start;
  _Unwind_Ptr LPStart;
  _Unwind_Ptr ttype_base;
  const unsigned char *TType;
  const unsigned char *action_table;
  unsigned char ttype_encoding;
  unsigned char call_site_encoding;
};

typedef struct lsda_header_info lsda_header_info;

static const unsigned char *
parse_lsda_header (context, p, info)
     _Unwind_Context *context;
     const unsigned char *p;
     lsda_header_info *info;
{
  _Unwind_Ptr tmp;
  unsigned char lpstart_encoding;

  info->Start = (context ? _Unwind_GetRegionStart (context) : 0);

  /* Find @LPStart, the base to which landing pad offsets are relative.  */
  lpstart_encoding = *p++;
  if (lpstart_encoding != DW_EH_PE_omit)
    p = read_encoded_value (context, lpstart_encoding, p, &info->LPStart);
  else
    info->LPStart = info->Start;

  /* Find @TType, the base of the handler and exception spec type data.  */
  info->ttype_encoding = *p++;
  if (info->ttype_encoding != DW_EH_PE_omit)
    {
      p = read_uleb128 (p, &tmp);
      info->TType = p + tmp;
    }
  else
    info->TType = 0;

  /* The encoding and length of the call-site table; the action table
     immediately follows.  */
  info->call_site_encoding = *p++;
  p = read_uleb128 (p, &tmp);
  info->action_table = p + tmp;

  return p;
}

static const _Unwind_Ptr
get_ttype_entry (context, info, i)
     _Unwind_Context *context;
     lsda_header_info *info;
     long i;
{
  _Unwind_Ptr ptr;

  i *= size_of_encoded_value (info->ttype_encoding);
  read_encoded_value (context, info->ttype_encoding, info->TType - i, &ptr);

  return ptr;
}

/* This is the structure of exception objects as built by the GNAT runtime
   library (a-except.adb). The layouts should exactly match, and the "common"
   header is mandated by the exception handling ABI.  */

struct _GNAT_Exception
{
  struct _Unwind_Exception common;
  _Unwind_Ptr id;
  char handled_by_others;
  char has_cleanup;
  char select_cleanups;
};


/* The two constants below are specific ttype identifiers for special
   exception ids. Their value is currently hardcoded at the gigi level
   (see N_Exception_Handler).  */

#define GNAT_OTHERS_ID      ((_Unwind_Ptr) 0x0)
#define GNAT_ALL_OTHERS_ID  ((_Unwind_Ptr) 0x1)


/* The DB stuff below is there for debugging purposes only.  */

#define DB_PHASES     0x1
#define DB_SEARCH     0x2
#define DB_ECLASS     0x4
#define DB_MATCH      0x8
#define DB_SAW        0x10
#define DB_FOUND      0x20
#define DB_INSTALL    0x40
#define DB_CALLS      0x80

#define AEHP_DB_SPECS \
(DB_PHASES | DB_SEARCH | DB_SAW | DB_FOUND | DB_INSTALL | DB_CALLS | DB_MATCH)

#undef AEHP_DB_SPECS

#ifdef AEHP_DB_SPECS
static int db_specs = AEHP_DB_SPECS;
#else
static int db_specs = 0;
#endif

#define START_DB(what) do { if (what & db_specs) {
#define END_DB(what)        } \
                           } while (0);

/* The "action" stuff below is also there for debugging purposes only.  */

typedef struct
{
  _Unwind_Action action;
  char * description;
} action_description_t;

static action_description_t action_descriptions[]
  = {{ _UA_SEARCH_PHASE,  "SEARCH_PHASE" },
     { _UA_CLEANUP_PHASE, "CLEANUP_PHASE" },
     { _UA_HANDLER_FRAME, "HANDLER_FRAME" },
     { _UA_FORCE_UNWIND,  "FORCE_UNWIND" },
     { -1, 0}};

static void
decode_actions (actions)
     _Unwind_Action actions;
{
  int i;

  action_description_t *a = action_descriptions;

  printf ("\n");
  for (; a->description != 0; a++)
    if (actions & a->action)
      printf ("%s ", a->description);

  printf (" : ");
}

/* The following is defined from a-except.adb. Its purpose is to enable
   automatic backtraces upon exception raise, as provided through the 
   GNAT.Traceback facilities.  */
extern void __gnat_notify_handled_exception PARAMS ((void *, bool, bool));

/* Below is the eh personality routine per se.  */

_Unwind_Reason_Code
__gnat_eh_personality (version, actions, exception_class, ue_header, context)
     int version;
     _Unwind_Action actions;
     _Unwind_Exception_Class exception_class;
     struct _Unwind_Exception *ue_header;
     struct _Unwind_Context *context;
{
  enum found_handler_type
  {
    found_nothing,
    found_terminate,
    found_cleanup,
    found_handler
  } found_type;
  lsda_header_info info;
  const unsigned char *language_specific_data;
  const unsigned char *action_record;
  const unsigned char *p;
  _Unwind_Ptr landing_pad, ip;
  int handler_switch_value;
  bool hit_others_handler;
  struct _GNAT_Exception *gnat_exception;

  if (version != 1)
    return _URC_FATAL_PHASE1_ERROR;

  START_DB (DB_PHASES);
  decode_actions (actions);
  END_DB (DB_PHASES);

  if (strcmp ((char *) &exception_class, "GNU") != 0
      || strcmp (((char *) &exception_class) + 4, "Ada") != 0)
    {
      START_DB (DB_SEARCH);
      printf ("              Exception Class doesn't match for ip = %p\n", ip);
      END_DB (DB_SEARCH);
      START_DB (DB_FOUND);
      printf ("              => FOUND nothing\n");
      END_DB (DB_FOUND);
      return _URC_CONTINUE_UNWIND;
    }

  gnat_exception = (struct _GNAT_Exception *) ue_header;

  START_DB (DB_PHASES);
  if (gnat_exception->select_cleanups)
    printf ("(select_cleanups) :\n");
  else
    printf (" :\n");
  END_DB (DB_PHASES);

  language_specific_data
    = (const unsigned char *) _Unwind_GetLanguageSpecificData (context);

  /* If no LSDA, then there are no handlers or cleanups.  */
  if (! language_specific_data)
    {
      ip = _Unwind_GetIP (context) - 1;

      START_DB (DB_SEARCH);
      printf ("              No Language Specific Data for ip = %p\n", ip);
      END_DB (DB_SEARCH);
      START_DB (DB_FOUND);
      printf ("              => FOUND nothing\n");
      END_DB (DB_FOUND);
      return _URC_CONTINUE_UNWIND;
    }

  /* Parse the LSDA header.  */
  p = parse_lsda_header (context, language_specific_data, &info);
  info.ttype_base = base_of_encoded_value (info.ttype_encoding, context);
  ip = _Unwind_GetIP (context) - 1;
  landing_pad = 0;
  action_record = 0;
  handler_switch_value = 0;

  /* Search the call-site table for the action associated with this IP.  */
  while (p < info.action_table)
    {
      _Unwind_Ptr cs_start, cs_len, cs_lp;
      _Unwind_Word cs_action;

      /* Note that all call-site encodings are "absolute" displacements.  */
      p = read_encoded_value (0, info.call_site_encoding, p, &cs_start);
      p = read_encoded_value (0, info.call_site_encoding, p, &cs_len);
      p = read_encoded_value (0, info.call_site_encoding, p, &cs_lp);
      p = read_uleb128 (p, &cs_action);

      /* The table is sorted, so if we've passed the ip, stop.  */
      if (ip < info.Start + cs_start)
 	p = info.action_table;
      else if (ip < info.Start + cs_start + cs_len)
	{
	  if (cs_lp)
	    landing_pad = info.LPStart + cs_lp;
	  if (cs_action)
	    action_record = info.action_table + cs_action - 1;
	  goto found_something;
	}
    }

  START_DB (DB_SEARCH);
  printf ("              No Action entry for ip = %p\n", ip);
  END_DB (DB_SEARCH);

  /* If ip is not present in the table, call terminate.  This is for
     a destructor inside a cleanup, or a library routine the compiler
     was not expecting to throw.

     found_type = 
     (actions & _UA_FORCE_UNWIND ? found_nothing : found_terminate);

     ??? Does this have a mapping in Ada semantics ?  */

  found_type = found_nothing;
  goto do_something;

 found_something:

  found_type = found_nothing;

  if (landing_pad == 0)
    {
      /* If ip is present, and has a null landing pad, there are
	 no cleanups or handlers to be run.  */
      START_DB (DB_SEARCH);
      printf ("              No Landing Pad for ip = %p\n", ip);
      END_DB (DB_SEARCH);
    }
  else if (action_record == 0)
    {
      START_DB (DB_SEARCH);
      printf ("              Null Action Record for ip = %p <===\n", ip);
      END_DB (DB_SEARCH);
    }
  else
    {
      signed long ar_filter, ar_disp;
      signed long cleanup_filter = 0;
      signed long handler_filter = 0;

      START_DB (DB_SEARCH);
      printf ("              Landing Pad + Action Record for ip = %p\n", ip);
      END_DB (DB_SEARCH);

      START_DB (DB_MATCH);
      printf ("              => Search for exception matching id %p\n", 
	      gnat_exception->id);
      END_DB (DB_MATCH);

      /* Otherwise we have a catch handler or exception specification.  */

      while (1)
	{
	  _Unwind_Word tmp;

	  p = action_record;
	  p = read_sleb128 (p, &tmp); ar_filter = tmp;
	  read_sleb128 (p, &tmp); ar_disp = tmp;

	  START_DB (DB_MATCH);
	  printf ("ar_filter  %d\n", ar_filter);
	  END_DB (DB_MATCH);

	  if (ar_filter == 0)
	    {
	      /* Zero filter values are cleanups. We should not be seeing
		 this for GNU-Ada though
		 saw_cleanup = true;  */
	      START_DB (DB_SEARCH);
	      printf ("              Null Filter for ip = %p <===\n", ip);
	      END_DB (DB_SEARCH);
	    }
	  else if (ar_filter > 0)
	    {
	      _Unwind_Ptr lp_id = get_ttype_entry (context, &info, ar_filter);

	      START_DB (DB_MATCH);
	      printf ("catch_type ");

	      switch (lp_id)
		{
		case GNAT_ALL_OTHERS_ID:
		  printf ("GNAT_ALL_OTHERS_ID\n");		
		  break;

		case GNAT_OTHERS_ID:
		  printf ("GNAT_OTHERS_ID\n");
		  break;

		default:
		  printf ("%p\n", lp_id);
		  break;
		}

	      END_DB (DB_MATCH);

	      if (lp_id == GNAT_ALL_OTHERS_ID)
		{
		  START_DB (DB_SAW);
		  printf ("              => SAW cleanup\n");
		  END_DB (DB_SAW);

		  cleanup_filter = ar_filter;
		  gnat_exception->has_cleanup = true;
		}

	      hit_others_handler
		= (lp_id == GNAT_OTHERS_ID
		   && gnat_exception->handled_by_others);

	      if (hit_others_handler || lp_id == gnat_exception->id)
		{
		  START_DB (DB_SAW);
		  printf ("              => SAW handler\n");
		  END_DB (DB_SAW);

		  handler_filter = ar_filter;     
		}
	    }
	  else
	    /* Negative filter values are for C++ exception specifications.
	       Should not be there for Ada :/  */
	    ;

	  if (actions & _UA_SEARCH_PHASE)
	    {
	      if (handler_filter)
		{
		  found_type = found_handler;
		  handler_switch_value = handler_filter;
		  break;
		}

	      if (cleanup_filter)
		found_type = found_cleanup;
	    }

	  if (actions & _UA_CLEANUP_PHASE)
	    {
	      if (handler_filter)
		{
		  found_type = found_handler;
		  handler_switch_value = handler_filter;
		  break;
		}

	      if (cleanup_filter)
		{
		  found_type = found_cleanup;
		  handler_switch_value = cleanup_filter;
		  break;
		}
	    }

	  if (ar_disp == 0)
	    break;

	  action_record = p + ar_disp;
	}
    }

 do_something:
  if (found_type == found_nothing)
    {
      START_DB (DB_FOUND);
      printf ("              => FOUND nothing\n");
      END_DB (DB_FOUND);

      return _URC_CONTINUE_UNWIND;
    }

  if (actions & _UA_SEARCH_PHASE)
    {
      START_DB (DB_FOUND);
      printf ("              => Computing return for SEARCH\n");
      END_DB (DB_FOUND);

      if (found_type == found_cleanup
	  && !gnat_exception->select_cleanups)
	{
	  START_DB (DB_FOUND);
	  printf ("              => FOUND cleanup\n");
	  END_DB (DB_FOUND);

	  return _URC_CONTINUE_UNWIND;
	}

      START_DB (DB_FOUND);
      printf ("              => FOUND handler\n");
      END_DB (DB_FOUND);

      return _URC_HANDLER_FOUND;
    }

 install_context:

   START_DB (DB_INSTALL);
   printf ("              => INSTALLING context for filter %d\n",
	   handler_switch_value);
   END_DB (DB_INSTALL);

   if (found_type == found_terminate)
     {
       /* Should not have this for Ada ?  */
       START_DB (DB_INSTALL);
       printf ("              => FOUND terminate <===\n");
       END_DB (DB_INSTALL);
     }


   /* Signal that we are going to enter a handler, which will typically
      enable the debugger to take control and possibly output an automatic
      backtrace. Note that we are supposed to provide the handler's entry
      point here but we don't have it.  */
  __gnat_notify_handled_exception ((void *)landing_pad, hit_others_handler,
				   true);

   /* The GNU-Ada exception handlers know how to find the exception
      occurrence without having to pass it as an argument so there
      is no need to feed any specific register with this information.

      This is why the two following lines are commented out.  */

   /* _Unwind_SetGR (context, __builtin_eh_return_data_regno (0),
      (_Unwind_Ptr) &xh->unwindHeader);  */

  _Unwind_SetGR (context, __builtin_eh_return_data_regno (1),
		 handler_switch_value);

  _Unwind_SetIP (context, landing_pad);

  return _URC_INSTALL_CONTEXT;
}


/* Stubs for the libgcc unwinding interface, to be imported by a-except.  */ 

#ifdef __USING_SJLJ_EXCEPTIONS__

_Unwind_Reason_Code
__gnat_Unwind_RaiseException (e)
     struct _Unwind_Exception *e;
{
  return _Unwind_SjLj_RaiseException (e);
}

#else
/* __USING_SJLJ_EXCEPTIONS__ not defined */

_Unwind_Reason_Code
__gnat_Unwind_RaiseException (e)
     struct _Unwind_Exception *e;
{
  return _Unwind_RaiseException (e);
}
 
#endif

#else
/* IN_RTS not defined */

/* The calls to the GCC runtime interface for exception raising are currently
   issued from a-except.adb, which is used by both the runtime library and
   the compiler. As the compiler binary is not linked against the GCC runtime
   library, we need a stub for this interface in the compiler case.  */

/* Since we don't link the compiler with a host libgcc, we should not be
   using the GCC eh mechanism for the compiler and so expect this function
   never to be called.  */

_Unwind_Reason_Code
__gnat_Unwind_RaiseException (e)
     struct _Unwind_Exception *e ATTRIBUTE_UNUSED;
{
  abort ();
}

#endif
