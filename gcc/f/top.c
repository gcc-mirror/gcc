/* top.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 1996, 1997, 1999 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

   Related Modules:
      None.

   Description:
      The GNU Fortran Front End.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "top.h"
#include "bad.h"
#include "bit.h"
#include "bld.h"
#include "com.h"
#include "data.h"
#include "equiv.h"
#include "expr.h"
#include "global.h"
#include "implic.h"
#include "info.h"
#include "intrin.h"
#include "lab.h"
#include "lex.h"
#include "malloc.h"
#include "name.h"
#include "src.h"
#include "st.h"
#include "storag.h"
#include "symbol.h"
#include "target.h"
#include "where.h"
#if FFECOM_targetCURRENT == FFECOM_targetGCC
#include "flags.j"
#include "toplev.j"
#endif

/* Externals defined here. */

int flag_traditional;		/* Shouldn't need this (C front end only)! */
bool ffe_is_do_internal_checks_ = FALSE;
bool ffe_is_90_ = FFETARGET_defaultIS_90;
bool ffe_is_automatic_ = FFETARGET_defaultIS_AUTOMATIC;
bool ffe_is_backslash_ = FFETARGET_defaultIS_BACKSLASH;
bool ffe_is_emulate_complex_ = FALSE;
bool ffe_is_underscoring_ = FFETARGET_defaultEXTERNAL_UNDERSCORED
  || FFETARGET_defaultUNDERSCORED_EXTERNAL_UNDERSCORED;
bool ffe_is_second_underscore_ = FFETARGET_defaultUNDERSCORED_EXTERNAL_UNDERSCORED;
bool ffe_is_debug_kludge_ = FALSE;
bool ffe_is_dollar_ok_ = FFETARGET_defaultIS_DOLLAR_OK;
bool ffe_is_f2c_ = FFETARGET_defaultIS_F2C;
bool ffe_is_f2c_library_ = FFETARGET_defaultIS_F2C_LIBRARY;
bool ffe_is_ffedebug_ = FALSE;
bool ffe_is_flatten_arrays_ = FALSE;
bool ffe_is_free_form_ = FFETARGET_defaultIS_FREE_FORM;
bool ffe_is_globals_ = TRUE;
bool ffe_is_init_local_zero_ = FFETARGET_defaultIS_INIT_LOCAL_ZERO;
bool ffe_is_mainprog_;		/* TRUE if current prog unit known to be
				   main. */
bool ffe_is_null_version_ = FALSE;
bool ffe_is_onetrip_ = FALSE;
bool ffe_is_silent_ = TRUE;
bool ffe_is_typeless_boz_ = FALSE;
bool ffe_is_pedantic_ = FFETARGET_defaultIS_PEDANTIC;
bool ffe_is_saveall_;		/* TRUE if mainprog or SAVE (no args) seen. */
bool ffe_is_ugly_args_ = TRUE;
bool ffe_is_ugly_assign_ = FALSE;	/* Try and store pointer to ASSIGN labels in INTEGER vars. */
bool ffe_is_ugly_assumed_ = FALSE;	/* DIMENSION X([...,]1) => DIMENSION X([...,]*) */
bool ffe_is_ugly_comma_ = FALSE;
bool ffe_is_ugly_complex_ = FALSE;
bool ffe_is_ugly_init_ = TRUE;
bool ffe_is_ugly_logint_ = FALSE;
bool ffe_is_version_ = FALSE;
bool ffe_is_vxt_ = FALSE;
bool ffe_is_warn_globals_ = TRUE;
bool ffe_is_warn_implicit_ = FALSE;
bool ffe_is_warn_surprising_ = FALSE;
bool ffe_is_zeros_ = FALSE;
ffeCase ffe_case_intrin_ = FFETARGET_defaultCASE_INTRIN;
ffeCase ffe_case_match_ = FFETARGET_defaultCASE_MATCH;
ffeCase ffe_case_source_ = FFETARGET_defaultCASE_SOURCE;
ffeCase ffe_case_symbol_ = FFETARGET_defaultCASE_SYMBOL;
ffeIntrinsicState ffe_intrinsic_state_badu77_ = FFE_intrinsicstateENABLED;
ffeIntrinsicState ffe_intrinsic_state_gnu_ = FFE_intrinsicstateENABLED;
ffeIntrinsicState ffe_intrinsic_state_f2c_ = FFE_intrinsicstateENABLED;
ffeIntrinsicState ffe_intrinsic_state_f90_ = FFE_intrinsicstateENABLED;
ffeIntrinsicState ffe_intrinsic_state_mil_ = FFE_intrinsicstateENABLED;
ffeIntrinsicState ffe_intrinsic_state_unix_ = FFE_intrinsicstateENABLED;
ffeIntrinsicState ffe_intrinsic_state_vxt_ = FFE_intrinsicstateENABLED;
int ffe_fixed_line_length_ = FFETARGET_defaultFIXED_LINE_LENGTH;
mallocPool ffe_file_pool_ = NULL;
mallocPool ffe_any_unit_pool_ = NULL;
mallocPool ffe_program_unit_pool_ = NULL;
ffeCounter ffe_count_0 = 0;
ffeCounter ffe_count_1 = 0;
ffeCounter ffe_count_2 = 0;
ffeCounter ffe_count_3 = 0;
ffeCounter ffe_count_4 = 0;
bool ffe_in_0 = FALSE;
bool ffe_in_1 = FALSE;
bool ffe_in_2 = FALSE;
bool ffe_in_3 = FALSE;
bool ffe_in_4 = FALSE;

/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module. */


/* Static functions (internal). */

static bool ffe_is_digit_string_ (char *s);

/* Internal macros. */

static bool
ffe_is_digit_string_ (char *s)
{
  char *p;

  for (p = s; ISDIGIT (*p); ++p)
    ;

  return (p != s) && (*p == '\0');
}

/* Handle command-line options.	 Returns 0 if unrecognized, 1 if
   recognized and handled.  */

int
ffe_decode_option (argc, argv)
     int argc ATTRIBUTE_UNUSED;
     char **argv;
{
  char *opt = argv[0];
  if (opt[0] != '-')
    return 0;
  if (opt[1] == 'f')
    {
      if (strcmp (&opt[2], "version") == 0)
	{
	  ffe_set_is_version (TRUE);
	  ffe_set_is_do_internal_checks (TRUE);
	}
      else if (strcmp (&opt[2], "null-version") == 0)
	ffe_set_is_null_version (TRUE);
      else if (strcmp (&opt[2], "f66") == 0)
	{
	  ffe_set_is_onetrip (TRUE);
	  ffe_set_is_ugly_assumed (TRUE);
	}
      else if (strcmp (&opt[2], "no-f66") == 0)
	{
	  ffe_set_is_onetrip (FALSE);
	  ffe_set_is_ugly_assumed (FALSE);
	}
      else if (strcmp (&opt[2], "f77") == 0)
	{
	  ffe_set_is_backslash (TRUE);
	  ffe_set_is_typeless_boz (FALSE);
	}
      else if (strcmp (&opt[2], "no-f77") == 0)
	{
	  ffe_set_is_backslash (FALSE);
	}
      else if (strcmp (&opt[2], "f90") == 0)
	ffe_set_is_90 (TRUE);
      else if (strcmp (&opt[2], "no-f90") == 0)
	ffe_set_is_90 (FALSE);
      else if (strcmp (&opt[2], "automatic") == 0)
	ffe_set_is_automatic (TRUE);
      else if (strcmp (&opt[2], "no-automatic") == 0)
	ffe_set_is_automatic (FALSE);
      else if (strcmp (&opt[2], "dollar-ok") == 0)
	ffe_set_is_dollar_ok (TRUE);
      else if (strcmp (&opt[2], "no-dollar-ok") == 0)
	ffe_set_is_dollar_ok (FALSE);
      else if (strcmp (&opt[2], "f2c") == 0)
	ffe_set_is_f2c (TRUE);
      else if (strcmp (&opt[2], "no-f2c") == 0)
	ffe_set_is_f2c (FALSE);
      else if (strcmp (&opt[2], "f2c-library") == 0)
	ffe_set_is_f2c_library (TRUE);
      else if (strcmp (&opt[2], "no-f2c-library") == 0)
	ffe_set_is_f2c_library (FALSE);
      else if (strcmp (&opt[2], "flatten-arrays") == 0)
	ffe_set_is_flatten_arrays (TRUE);
      else if (strcmp (&opt[2], "no-flatten-arrays") == 0)
	ffe_set_is_flatten_arrays (FALSE);
      else if (strcmp (&opt[2], "free-form") == 0)
	ffe_set_is_free_form (TRUE);
      else if (strcmp (&opt[2], "no-free-form") == 0)
	ffe_set_is_free_form (FALSE);
      else if (strcmp (&opt[2], "fixed-form") == 0)
	ffe_set_is_free_form (FALSE);
      else if (strcmp (&opt[2], "no-fixed-form") == 0)
	ffe_set_is_free_form (TRUE);
      else if (strcmp (&opt[2], "pedantic") == 0)
	ffe_set_is_pedantic (TRUE);
      else if (strcmp (&opt[2], "no-pedantic") == 0)
	ffe_set_is_pedantic (FALSE);
      else if (strcmp (&opt[2], "vxt") == 0)
	ffe_set_is_vxt (TRUE);
      else if (strcmp (&opt[2], "not-vxt") == 0)
	ffe_set_is_vxt (FALSE);
      else if (strcmp (&opt[2], "vxt-not-f90") == 0)
	warning ("%s no longer supported -- try -fvxt", opt);
      else if (strcmp (&opt[2], "f90-not-vxt") == 0)
	warning ("%s no longer supported -- try -fno-vxt -ff90", opt);
      else if (strcmp (&opt[2], "no-ugly") == 0)
	{
	  ffe_set_is_ugly_args (FALSE);
	  ffe_set_is_ugly_assign (FALSE);
	  ffe_set_is_ugly_assumed (FALSE);
	  ffe_set_is_ugly_comma (FALSE);
	  ffe_set_is_ugly_complex (FALSE);
	  ffe_set_is_ugly_init (FALSE);
	  ffe_set_is_ugly_logint (FALSE);
	}
      else if (strcmp (&opt[2], "ugly-args") == 0)
	ffe_set_is_ugly_args (TRUE);
      else if (strcmp (&opt[2], "no-ugly-args") == 0)
	ffe_set_is_ugly_args (FALSE);
      else if (strcmp (&opt[2], "ugly-assign") == 0)
	ffe_set_is_ugly_assign (TRUE);
      else if (strcmp (&opt[2], "no-ugly-assign") == 0)
	ffe_set_is_ugly_assign (FALSE);
      else if (strcmp (&opt[2], "ugly-assumed") == 0)
	ffe_set_is_ugly_assumed (TRUE);
      else if (strcmp (&opt[2], "no-ugly-assumed") == 0)
	ffe_set_is_ugly_assumed (FALSE);
      else if (strcmp (&opt[2], "ugly-comma") == 0)
	ffe_set_is_ugly_comma (TRUE);
      else if (strcmp (&opt[2], "no-ugly-comma") == 0)
	ffe_set_is_ugly_comma (FALSE);
      else if (strcmp (&opt[2], "ugly-complex") == 0)
	ffe_set_is_ugly_complex (TRUE);
      else if (strcmp (&opt[2], "no-ugly-complex") == 0)
	ffe_set_is_ugly_complex (FALSE);
      else if (strcmp (&opt[2], "ugly-init") == 0)
	ffe_set_is_ugly_init (TRUE);
      else if (strcmp (&opt[2], "no-ugly-init") == 0)
	ffe_set_is_ugly_init (FALSE);
      else if (strcmp (&opt[2], "ugly-logint") == 0)
	ffe_set_is_ugly_logint (TRUE);
      else if (strcmp (&opt[2], "no-ugly-logint") == 0)
	ffe_set_is_ugly_logint (FALSE);
      else if (strcmp (&opt[2], "xyzzy") == 0)
	ffe_set_is_ffedebug (TRUE);
      else if (strcmp (&opt[2], "no-xyzzy") == 0)
	ffe_set_is_ffedebug (FALSE);
      else if (strcmp (&opt[2], "init-local-zero") == 0)
	ffe_set_is_init_local_zero (TRUE);
      else if (strcmp (&opt[2], "no-init-local-zero") == 0)
	ffe_set_is_init_local_zero (FALSE);
      else if (strcmp (&opt[2], "emulate-complex") == 0)
	ffe_set_is_emulate_complex (TRUE);
      else if (strcmp (&opt[2], "no-emulate-complex") == 0)
	ffe_set_is_emulate_complex (FALSE);
      else if (strcmp (&opt[2], "backslash") == 0)
	ffe_set_is_backslash (TRUE);
      else if (strcmp (&opt[2], "no-backslash") == 0)
	ffe_set_is_backslash (FALSE);
      else if (strcmp (&opt[2], "underscoring") == 0)
	ffe_set_is_underscoring (TRUE);
      else if (strcmp (&opt[2], "no-underscoring") == 0)
	ffe_set_is_underscoring (FALSE);
      else if (strcmp (&opt[2], "second-underscore") == 0)
	ffe_set_is_second_underscore (TRUE);
      else if (strcmp (&opt[2], "no-second-underscore") == 0)
	ffe_set_is_second_underscore (FALSE);
      else if (strcmp (&opt[2], "zeros") == 0)
	ffe_set_is_zeros (TRUE);
      else if (strcmp (&opt[2], "no-zeros") == 0)
	ffe_set_is_zeros (FALSE);
      else if (strcmp (&opt[2], "debug-kludge") == 0)
	ffe_set_is_debug_kludge (TRUE);
      else if (strcmp (&opt[2], "no-debug-kludge") == 0)
	ffe_set_is_debug_kludge (FALSE);
      else if (strcmp (&opt[2], "onetrip") == 0)
	ffe_set_is_onetrip (TRUE);
      else if (strcmp (&opt[2], "no-onetrip") == 0)
	ffe_set_is_onetrip (FALSE);
      else if (strcmp (&opt[2], "silent") == 0)
	ffe_set_is_silent (TRUE);
      else if (strcmp (&opt[2], "no-silent") == 0)
	ffe_set_is_silent (FALSE);
      else if (strcmp (&opt[2], "globals") == 0)
	ffe_set_is_globals (TRUE);
      else if (strcmp (&opt[2], "no-globals") == 0)
	ffe_set_is_globals (FALSE);
      else if (strcmp (&opt[2], "fortran-bounds-check") == 0)
	flag_bounds_check = TRUE;
      else if (strcmp (&opt[2], "no-fortran-bounds-check") == 0)
	flag_bounds_check = FALSE;
      else if (strcmp (&opt[2], "typeless-boz") == 0)
	ffe_set_is_typeless_boz (TRUE);
      else if (strcmp (&opt[2], "no-typeless-boz") == 0)
	ffe_set_is_typeless_boz (FALSE);
      else if (strcmp (&opt[2], "intrin-case-initcap") == 0)
	ffe_set_case_intrin (FFE_caseINITCAP);
      else if (strcmp (&opt[2], "intrin-case-upper") == 0)
	ffe_set_case_intrin (FFE_caseUPPER);
      else if (strcmp (&opt[2], "intrin-case-lower") == 0)
	ffe_set_case_intrin (FFE_caseLOWER);
      else if (strcmp (&opt[2], "intrin-case-any") == 0)
	ffe_set_case_intrin (FFE_caseNONE);
      else if (strcmp (&opt[2], "match-case-initcap") == 0)
	ffe_set_case_match (FFE_caseINITCAP);
      else if (strcmp (&opt[2], "match-case-upper") == 0)
	ffe_set_case_match (FFE_caseUPPER);
      else if (strcmp (&opt[2], "match-case-lower") == 0)
	ffe_set_case_match (FFE_caseLOWER);
      else if (strcmp (&opt[2], "match-case-any") == 0)
	ffe_set_case_match (FFE_caseNONE);
      else if (strcmp (&opt[2], "source-case-upper") == 0)
	ffe_set_case_source (FFE_caseUPPER);
      else if (strcmp (&opt[2], "source-case-lower") == 0)
	ffe_set_case_source (FFE_caseLOWER);
      else if (strcmp (&opt[2], "source-case-preserve") == 0)
	ffe_set_case_source (FFE_caseNONE);
      else if (strcmp (&opt[2], "symbol-case-initcap") == 0)
	ffe_set_case_symbol (FFE_caseINITCAP);
      else if (strcmp (&opt[2], "symbol-case-upper") == 0)
	ffe_set_case_symbol (FFE_caseUPPER);
      else if (strcmp (&opt[2], "symbol-case-lower") == 0)
	ffe_set_case_symbol (FFE_caseLOWER);
      else if (strcmp (&opt[2], "symbol-case-any") == 0)
	ffe_set_case_symbol (FFE_caseNONE);
      else if (strcmp (&opt[2], "case-strict-upper") == 0)
	{
	  ffe_set_case_intrin (FFE_caseUPPER);
	  ffe_set_case_match (FFE_caseUPPER);
	  ffe_set_case_source (FFE_caseNONE);
	  ffe_set_case_symbol (FFE_caseUPPER);
	}
      else if (strcmp (&opt[2], "case-strict-lower") == 0)
	{
	  ffe_set_case_intrin (FFE_caseLOWER);
	  ffe_set_case_match (FFE_caseLOWER);
	  ffe_set_case_source (FFE_caseNONE);
	  ffe_set_case_symbol (FFE_caseLOWER);
	}
      else if (strcmp (&opt[2], "case-initcap") == 0)
	{
	  ffe_set_case_intrin (FFE_caseINITCAP);
	  ffe_set_case_match (FFE_caseINITCAP);
	  ffe_set_case_source (FFE_caseNONE);
	  ffe_set_case_symbol (FFE_caseINITCAP);
	}
      else if (strcmp (&opt[2], "case-upper") == 0)
	{
	  ffe_set_case_intrin (FFE_caseNONE);
	  ffe_set_case_match (FFE_caseNONE);
	  ffe_set_case_source (FFE_caseUPPER);
	  ffe_set_case_symbol (FFE_caseNONE);
	}
      else if (strcmp (&opt[2], "case-lower") == 0)
	{
	  ffe_set_case_intrin (FFE_caseNONE);
	  ffe_set_case_match (FFE_caseNONE);
	  ffe_set_case_source (FFE_caseLOWER);
	  ffe_set_case_symbol (FFE_caseNONE);
	}
      else if (strcmp (&opt[2], "case-preserve") == 0)
	{
	  ffe_set_case_intrin (FFE_caseNONE);
	  ffe_set_case_match (FFE_caseNONE);
	  ffe_set_case_source (FFE_caseNONE);
	  ffe_set_case_symbol (FFE_caseNONE);
	}
      else if (strcmp (&opt[2], "badu77-intrinsics-delete") == 0)
	ffe_set_intrinsic_state_badu77 (FFE_intrinsicstateDELETED);
      else if (strcmp (&opt[2], "badu77-intrinsics-hide") == 0)
	ffe_set_intrinsic_state_badu77 (FFE_intrinsicstateHIDDEN);
      else if (strcmp (&opt[2], "badu77-intrinsics-disable") == 0)
	ffe_set_intrinsic_state_badu77 (FFE_intrinsicstateDISABLED);
      else if (strcmp (&opt[2], "badu77-intrinsics-enable") == 0)
	ffe_set_intrinsic_state_badu77 (FFE_intrinsicstateENABLED);
      else if (strcmp (&opt[2], "gnu-intrinsics-delete") == 0)
	ffe_set_intrinsic_state_gnu (FFE_intrinsicstateDELETED);
      else if (strcmp (&opt[2], "gnu-intrinsics-hide") == 0)
	ffe_set_intrinsic_state_gnu (FFE_intrinsicstateHIDDEN);
      else if (strcmp (&opt[2], "gnu-intrinsics-disable") == 0)
	ffe_set_intrinsic_state_gnu (FFE_intrinsicstateDISABLED);
      else if (strcmp (&opt[2], "gnu-intrinsics-enable") == 0)
	ffe_set_intrinsic_state_gnu (FFE_intrinsicstateENABLED);
      else if (strcmp (&opt[2], "f2c-intrinsics-delete") == 0)
	ffe_set_intrinsic_state_f2c (FFE_intrinsicstateDELETED);
      else if (strcmp (&opt[2], "f2c-intrinsics-hide") == 0)
	ffe_set_intrinsic_state_f2c (FFE_intrinsicstateHIDDEN);
      else if (strcmp (&opt[2], "f2c-intrinsics-disable") == 0)
	ffe_set_intrinsic_state_f2c (FFE_intrinsicstateDISABLED);
      else if (strcmp (&opt[2], "f2c-intrinsics-enable") == 0)
	ffe_set_intrinsic_state_f2c (FFE_intrinsicstateENABLED);
      else if (strcmp (&opt[2], "f90-intrinsics-delete") == 0)
	ffe_set_intrinsic_state_f90 (FFE_intrinsicstateDELETED);
      else if (strcmp (&opt[2], "f90-intrinsics-hide") == 0)
	ffe_set_intrinsic_state_f90 (FFE_intrinsicstateHIDDEN);
      else if (strcmp (&opt[2], "f90-intrinsics-disable") == 0)
	ffe_set_intrinsic_state_f90 (FFE_intrinsicstateDISABLED);
      else if (strcmp (&opt[2], "f90-intrinsics-enable") == 0)
	ffe_set_intrinsic_state_f90 (FFE_intrinsicstateENABLED);
      else if (strcmp (&opt[2], "mil-intrinsics-delete") == 0)
	ffe_set_intrinsic_state_mil (FFE_intrinsicstateDELETED);
      else if (strcmp (&opt[2], "mil-intrinsics-hide") == 0)
	ffe_set_intrinsic_state_mil (FFE_intrinsicstateHIDDEN);
      else if (strcmp (&opt[2], "mil-intrinsics-disable") == 0)
	ffe_set_intrinsic_state_mil (FFE_intrinsicstateDISABLED);
      else if (strcmp (&opt[2], "mil-intrinsics-enable") == 0)
	ffe_set_intrinsic_state_mil (FFE_intrinsicstateENABLED);
      else if (strcmp (&opt[2], "unix-intrinsics-delete") == 0)
	ffe_set_intrinsic_state_unix (FFE_intrinsicstateDELETED);
      else if (strcmp (&opt[2], "unix-intrinsics-hide") == 0)
	ffe_set_intrinsic_state_unix (FFE_intrinsicstateHIDDEN);
      else if (strcmp (&opt[2], "unix-intrinsics-disable") == 0)
	ffe_set_intrinsic_state_unix (FFE_intrinsicstateDISABLED);
      else if (strcmp (&opt[2], "unix-intrinsics-enable") == 0)
	ffe_set_intrinsic_state_unix (FFE_intrinsicstateENABLED);
      else if (strcmp (&opt[2], "vxt-intrinsics-delete") == 0)
	ffe_set_intrinsic_state_vxt (FFE_intrinsicstateDELETED);
      else if (strcmp (&opt[2], "vxt-intrinsics-hide") == 0)
	ffe_set_intrinsic_state_vxt (FFE_intrinsicstateHIDDEN);
      else if (strcmp (&opt[2], "vxt-intrinsics-disable") == 0)
	ffe_set_intrinsic_state_vxt (FFE_intrinsicstateDISABLED);
      else if (strcmp (&opt[2], "vxt-intrinsics-enable") == 0)
	ffe_set_intrinsic_state_vxt (FFE_intrinsicstateENABLED);
      else if (strncmp (&opt[2], "fixed-line-length-",
			strlen ("fixed-line-length-")) == 0)
	{
	  char *len = &opt[2] + strlen ("fixed-line-length-");

	  if (strcmp (len, "none") == 0)
	    ffe_set_fixed_line_length (0);
	  else if (ffe_is_digit_string_ (len))
	    ffe_set_fixed_line_length (atol (len));
	  else
	    return 0;
	}
      else
	return 0;
    }
  else if (opt[1] == 'W')
    {
      if (!strcmp (&opt[2], "comment"))
	; /* cpp handles this one.  */
      else if (!strcmp (&opt[2], "no-comment"))
	; /* cpp handles this one.  */
      else if (!strcmp (&opt[2], "comments"))
	; /* cpp handles this one.  */
      else if (!strcmp (&opt[2], "no-comments"))
	; /* cpp handles this one.  */
      else if (!strcmp (&opt[2], "trigraphs"))
	; /* cpp handles this one.  */
      else if (!strcmp (&opt[2], "no-trigraphs"))
	; /* cpp handles this one.  */
      else if (!strcmp (&opt[2], "import"))
	; /* cpp handles this one.  */
      else if (!strcmp (&opt[2], "no-import"))
	; /* cpp handles this one.  */
      else if (!strcmp (&opt[2], "globals"))
	ffe_set_is_warn_globals (TRUE);
      else if (!strcmp (&opt[2], "no-globals"))
	ffe_set_is_warn_globals (FALSE);
      else if (!strcmp (&opt[2], "implicit"))
	ffe_set_is_warn_implicit (TRUE);
      else if (!strcmp (&opt[2], "no-implicit"))
	ffe_set_is_warn_implicit (FALSE);
      else if (!strcmp (&opt[2], "surprising"))
	ffe_set_is_warn_surprising (TRUE);
      else if (!strcmp (&opt[2], "no-surprising"))
	ffe_set_is_warn_surprising (FALSE);
      else if (!strcmp (&opt[2], "all"))
	{
	  /* We save the value of warn_uninitialized, since if they put
	     -Wuninitialized on the command line, we need to generate a
	     warning about not using it without also specifying -O.  */
	  if (warn_uninitialized != 1)
	    warn_uninitialized = 2;
	  warn_unused = 1;
	}
      else
	return 0;
    }
  else if (opt[1] == 'I')
    return ffecom_decode_include_option (&opt[2]);
  else
    return 0;

  return 1;
}

/* Run the FFE on a source file (not an INCLUDEd file).

   Runs the whole shebang.

   Prepare and invoke the appropriate lexer.  */

void
ffe_file (ffewhereFile wf, FILE *f)
{
  ffe_init_1 ();
  ffelex_set_handler ((ffelexHandler) ffest_first);
  ffewhere_file_set (wf, TRUE, 0);
  if (ffe_is_free_form_)
    ffelex_file_free (wf, f);
  else
    ffelex_file_fixed (wf, f);
  ffest_eof ();
  ffe_terminate_1 ();
}

/* ffe_init_0 -- Initialize the FFE per image invocation

   ffe_init_0();

   Performs per-image invocation.  */

void
ffe_init_0 ()
{
  ++ffe_count_0;
  ffe_in_0 = TRUE;

  ffebad_init_0 ();
  ffebit_init_0 ();
  ffebld_init_0 ();
  ffecom_init_0 ();
  ffedata_init_0 ();
  ffeequiv_init_0 ();
  ffeexpr_init_0 ();
  ffeglobal_init_0 ();
  ffeimplic_init_0 ();
  ffeinfo_init_0 ();
  ffeintrin_init_0 ();
  ffelab_init_0 ();
  ffelex_init_0 ();
  ffename_init_0 ();
  ffesrc_init_0 ();
  ffest_init_0 ();
  ffestorag_init_0 ();
  ffesymbol_init_0 ();
  ffetarget_init_0 ();
  ffetype_init_0 ();
  ffewhere_init_0 ();
}

/* ffe_init_1 -- Initialize the FFE per source file

   ffe_init_1();

   Performs per-source-file invocation (not including INCLUDEd files).	*/

void
ffe_init_1 ()
{
  ++ffe_count_1;
  ffe_in_1 = TRUE;

  assert (ffe_file_pool_ == NULL);
  ffe_file_pool_ = malloc_pool_new ("File", malloc_pool_image (), 1024);

  ffebad_init_1 ();
  ffebit_init_1 ();
  ffebld_init_1 ();
  ffecom_init_1 ();
  ffedata_init_1 ();
  ffeequiv_init_1 ();
  ffeexpr_init_1 ();
  ffeglobal_init_1 ();
  ffeimplic_init_1 ();
  ffeinfo_init_1 ();
  ffeintrin_init_1 ();
  ffelab_init_1 ();
  ffelex_init_1 ();
  ffename_init_1 ();
  ffesrc_init_1 ();
  ffest_init_1 ();
  ffestorag_init_1 ();
  ffesymbol_init_1 ();
  ffetarget_init_1 ();
  ffetype_init_1 ();
  ffewhere_init_1 ();

  ffe_init_2 ();
}

/* ffe_init_2 -- Initialize the FFE per outer program unit

   ffe_init_2();

   Performs per-program-unit invocation.  */

void
ffe_init_2 ()
{
  ++ffe_count_2;
  ffe_in_2 = TRUE;

  assert (ffe_program_unit_pool_ == NULL);
  ffe_program_unit_pool_ = malloc_pool_new ("Program unit", ffe_file_pool_, 1024);
  ffe_is_mainprog_ = FALSE;
  ffe_is_saveall_ = !ffe_is_automatic_;

  ffebad_init_2 ();
  ffebit_init_2 ();
  ffebld_init_2 ();
  ffecom_init_2 ();
  ffedata_init_2 ();
  ffeequiv_init_2 ();
  ffeexpr_init_2 ();
  ffeglobal_init_2 ();
  ffeimplic_init_2 ();
  ffeinfo_init_2 ();
  ffeintrin_init_2 ();
  ffelab_init_2 ();
  ffelex_init_2 ();
  ffename_init_2 ();
  ffesrc_init_2 ();
  ffest_init_2 ();
  ffestorag_init_2 ();
  ffesymbol_init_2 ();
  ffetarget_init_2 ();
  ffetype_init_2 ();
  ffewhere_init_2 ();

  ffe_init_3 ();
}

/* ffe_init_3 -- Initialize the FFE per any program unit

   ffe_init_3();

   Performs per-any-unit initialization; does NOT do
   per-statement-function-definition initialization (i.e. the chain
   of inits, from 0-3, breaks here; level 4 must be invoked independently).  */

void
ffe_init_3 ()
{
  ++ffe_count_3;
  ffe_in_3 = TRUE;

  assert (ffe_any_unit_pool_ == NULL);
  ffe_any_unit_pool_ = malloc_pool_new ("Any unit", ffe_program_unit_pool_, 1024);

  ffebad_init_3 ();
  ffebit_init_3 ();
  ffebld_init_3 ();
  ffecom_init_3 ();
  ffedata_init_3 ();
  ffeequiv_init_3 ();
  ffeexpr_init_3 ();
  ffeglobal_init_3 ();
  ffeimplic_init_3 ();
  ffeinfo_init_3 ();
  ffeintrin_init_3 ();
  ffelab_init_3 ();
  ffelex_init_3 ();
  ffename_init_3 ();
  ffesrc_init_3 ();
  ffest_init_3 ();
  ffestorag_init_3 ();
  ffesymbol_init_3 ();
  ffetarget_init_3 ();
  ffetype_init_3 ();
  ffewhere_init_3 ();
}

/* ffe_init_4 -- Initialize the FFE per statement function definition

   ffe_init_4();  */

void
ffe_init_4 ()
{
  ++ffe_count_4;
  ffe_in_4 = TRUE;

  ffebad_init_4 ();
  ffebit_init_4 ();
  ffebld_init_4 ();
  ffecom_init_4 ();
  ffedata_init_4 ();
  ffeequiv_init_4 ();
  ffeexpr_init_4 ();
  ffeglobal_init_4 ();
  ffeimplic_init_4 ();
  ffeinfo_init_4 ();
  ffeintrin_init_4 ();
  ffelab_init_4 ();
  ffelex_init_4 ();
  ffename_init_4 ();
  ffesrc_init_4 ();
  ffest_init_4 ();
  ffestorag_init_4 ();
  ffesymbol_init_4 ();
  ffetarget_init_4 ();
  ffetype_init_4 ();
  ffewhere_init_4 ();
}

/* ffe_terminate_0 -- Terminate the FFE prior to image termination

   ffe_terminate_0();  */

void
ffe_terminate_0 ()
{
  ffe_count_1 = 0;
  ffe_in_0 = FALSE;

  ffebad_terminate_0 ();
  ffebit_terminate_0 ();
  ffebld_terminate_0 ();
  ffecom_terminate_0 ();
  ffedata_terminate_0 ();
  ffeequiv_terminate_0 ();
  ffeexpr_terminate_0 ();
  ffeglobal_terminate_0 ();
  ffeimplic_terminate_0 ();
  ffeinfo_terminate_0 ();
  ffeintrin_terminate_0 ();
  ffelab_terminate_0 ();
  ffelex_terminate_0 ();
  ffename_terminate_0 ();
  ffesrc_terminate_0 ();
  ffest_terminate_0 ();
  ffestorag_terminate_0 ();
  ffesymbol_terminate_0 ();
  ffetarget_terminate_0 ();
  ffetype_terminate_0 ();
  ffewhere_terminate_0 ();
}

/* ffe_terminate_1 -- Terminate the FFE after seeing source file EOF

   ffe_terminate_1();  */

void
ffe_terminate_1 ()
{
  ffe_count_2 = 0;
  ffe_in_1 = FALSE;

  ffe_terminate_2 ();

  ffebad_terminate_1 ();
  ffebit_terminate_1 ();
  ffebld_terminate_1 ();
  ffecom_terminate_1 ();
  ffedata_terminate_1 ();
  ffeequiv_terminate_1 ();
  ffeexpr_terminate_1 ();
  ffeglobal_terminate_1 ();
  ffeimplic_terminate_1 ();
  ffeinfo_terminate_1 ();
  ffeintrin_terminate_1 ();
  ffelab_terminate_1 ();
  ffelex_terminate_1 ();
  ffename_terminate_1 ();
  ffesrc_terminate_1 ();
  ffest_terminate_1 ();
  ffestorag_terminate_1 ();
  ffesymbol_terminate_1 ();
  ffetarget_terminate_1 ();
  ffetype_terminate_1 ();
  ffewhere_terminate_1 ();

  assert (ffe_file_pool_ != NULL);
  malloc_pool_kill (ffe_file_pool_);
  ffe_file_pool_ = NULL;
}

/* ffe_terminate_2 -- Terminate the FFE after seeing outer program unit END

   ffe_terminate_2();  */

void
ffe_terminate_2 ()
{
  ffe_count_3 = 0;
  ffe_in_2 = FALSE;

  ffe_terminate_3 ();

  ffebad_terminate_2 ();
  ffebit_terminate_2 ();
  ffebld_terminate_2 ();
  ffecom_terminate_2 ();
  ffedata_terminate_2 ();
  ffeequiv_terminate_2 ();
  ffeexpr_terminate_2 ();
  ffeglobal_terminate_2 ();
  ffeimplic_terminate_2 ();
  ffeinfo_terminate_2 ();
  ffeintrin_terminate_2 ();
  ffelab_terminate_2 ();
  ffelex_terminate_2 ();
  ffename_terminate_2 ();
  ffesrc_terminate_2 ();
  ffest_terminate_2 ();
  ffestorag_terminate_2 ();
  ffesymbol_terminate_2 ();
  ffetarget_terminate_2 ();
  ffetype_terminate_2 ();
  ffewhere_terminate_2 ();

  assert (ffe_program_unit_pool_ != NULL);
  malloc_pool_kill (ffe_program_unit_pool_);
  ffe_program_unit_pool_ = NULL;
}

/* ffe_terminate_3 -- Terminate the FFE after seeing any program unit END

   ffe_terminate_3();  */

void
ffe_terminate_3 ()
{
  ffe_count_4 = 0;
  ffe_in_3 = FALSE;

  ffebad_terminate_3 ();
  ffebit_terminate_3 ();
  ffebld_terminate_3 ();
  ffecom_terminate_3 ();
  ffedata_terminate_3 ();
  ffeequiv_terminate_3 ();
  ffeexpr_terminate_3 ();
  ffeglobal_terminate_3 ();
  ffeimplic_terminate_3 ();
  ffeinfo_terminate_3 ();
  ffeintrin_terminate_3 ();
  ffelab_terminate_3 ();
  ffelex_terminate_3 ();
  ffename_terminate_3 ();
  ffesrc_terminate_3 ();
  ffest_terminate_3 ();
  ffestorag_terminate_3 ();
  ffesymbol_terminate_3 ();
  ffetarget_terminate_3 ();
  ffetype_terminate_3 ();
  ffewhere_terminate_3 ();

  assert (ffe_any_unit_pool_ != NULL);
  malloc_pool_kill (ffe_any_unit_pool_);
  ffe_any_unit_pool_ = NULL;
}

/* ffe_terminate_4 -- Terminate the FFE after seeing sfunc def expression

   ffe_terminate_4();  */

void
ffe_terminate_4 ()
{
  ffe_in_4 = FALSE;

  ffebad_terminate_4 ();
  ffebit_terminate_4 ();
  ffebld_terminate_4 ();
  ffecom_terminate_4 ();
  ffedata_terminate_4 ();
  ffeequiv_terminate_4 ();
  ffeexpr_terminate_4 ();
  ffeglobal_terminate_4 ();
  ffeimplic_terminate_4 ();
  ffeinfo_terminate_4 ();
  ffeintrin_terminate_4 ();
  ffelab_terminate_4 ();
  ffelex_terminate_4 ();
  ffename_terminate_4 ();
  ffesrc_terminate_4 ();
  ffest_terminate_4 ();
  ffestorag_terminate_4 ();
  ffesymbol_terminate_4 ();
  ffetarget_terminate_4 ();
  ffetype_terminate_4 ();
  ffewhere_terminate_4 ();
}
