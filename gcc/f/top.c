/* top.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 1996, 1997, 1999, 2001, 2003
   Free Software Foundation, Inc.
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
#include "options.h"
#include "opts.h"
#include "src.h"
#include "st.h"
#include "storag.h"
#include "symbol.h"
#include "target.h"
#include "where.h"
#include "flags.h"
#include "toplev.h"

/* Externals defined here. */

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

static bool ffe_is_digit_string_ (const char *s);

/* Internal macros. */

static bool
ffe_is_digit_string_ (const char *s)
{
  const char *p;

  for (p = s; ISDIGIT (*p); ++p)
    ;

  return (p != s) && (*p == '\0');
}

/* Get ready for options handling.  */
unsigned int
ffe_init_options (unsigned int argc ATTRIBUTE_UNUSED,
		  const char **argv ATTRIBUTE_UNUSED)
{
  /* Set default options for Fortran.  */
  flag_move_all_movables = 1;
  flag_reduce_all_givs = 1;
  flag_argument_noalias = 2;
  flag_merge_constants = 2;
  flag_errno_math = 0;
  flag_complex_divide_method = 1;

  return CL_F77;
}

/* Handle command-line options.	 Returns 0 if unrecognized, 1 if
   recognized and handled.  */
int
ffe_handle_option (size_t scode, const char *arg, int value)
{
  enum opt_code code = (enum opt_code) scode;

  switch (code)
    {
    default:
      abort();

    case OPT_fversion:
      ffe_set_is_version (TRUE);
      ffe_set_is_do_internal_checks (TRUE);
      break;

    case OPT_ff66:
      ffe_set_is_onetrip (value);
      ffe_set_is_ugly_assumed (value);
      break;

    case OPT_ff77:
      ffe_set_is_backslash (value);
      if (value)
	ffe_set_is_typeless_boz (FALSE);
      break;

    case OPT_ff90:
      ffe_set_is_90 (value);
      break;

    case OPT_fautomatic:
      ffe_set_is_automatic (value);
      break;

    case OPT_fdollar_ok:
      ffe_set_is_dollar_ok (value);
      break;

    case OPT_ff2c:
      ffe_set_is_f2c (value);
      break;

    case OPT_ff2c_library:
      ffe_set_is_f2c_library (value);
      break;

    case OPT_fflatten_arrays:
      ffe_set_is_f2c_library (value);
      break;

    case OPT_ffree_form:
      ffe_set_is_free_form (value);
      break;

    case OPT_ffixed_form:
      ffe_set_is_free_form (!value);
      break;

    case OPT_fpedantic:
      ffe_set_is_pedantic (value);
      break;

    case OPT_fvxt:
      ffe_set_is_vxt (value);
      break;

    case OPT_fvxt_not_f90:
      warning ("-fvxt-not-f90 no longer supported -- try -fvxt");
      break;

    case OPT_ff90_not_vxt:
      warning ("-ff90-not-vxt no longer supported -- try -fno-vxt -ff90");
      break;

    case OPT_fugly:
      ffe_set_is_ugly_args (value);
      ffe_set_is_ugly_assign (value);
      ffe_set_is_ugly_assumed (value);
      ffe_set_is_ugly_comma (value);
      ffe_set_is_ugly_complex (value);
      ffe_set_is_ugly_init (value);
      ffe_set_is_ugly_logint (value);
      break;

    case OPT_fugly_args:
      ffe_set_is_ugly_args (value);
      break;

    case OPT_fugly_assign:
      ffe_set_is_ugly_assign (value);
      break;

    case OPT_fugly_assumed:
      ffe_set_is_ugly_assumed (value);
      break;

    case OPT_fugly_comma:
      ffe_set_is_ugly_comma (value);
      break;

    case OPT_fugly_complex:
      ffe_set_is_ugly_complex (value);
      break;

    case OPT_fugly_init:
      ffe_set_is_ugly_init (value);
      break;

    case OPT_fugly_logint:
      ffe_set_is_ugly_logint (value);
      break;

    case OPT_fxyzzy:
      ffe_set_is_ffedebug (value);
      break;

    case OPT_finit_local_zero:
      ffe_set_is_init_local_zero (value);
      break;

    case OPT_femulate_complex:
      ffe_set_is_emulate_complex (value);
      break;

    case OPT_fbackslash:
      ffe_set_is_backslash (value);
      break;

    case OPT_funderscoring:
      ffe_set_is_underscoring (value);
      break;

    case OPT_fsecond_underscore:
      ffe_set_is_second_underscore (value);
      break;

    case OPT_fzeros:
      ffe_set_is_zeros (value);
      break;

    case OPT_fdebug_kludge:
      warning ("-fdebug-kludge is disabled, use normal debugging flags");
      break;

    case OPT_fonetrip:
      ffe_set_is_onetrip (value);
      break;

    case OPT_fsilent:
      ffe_set_is_silent (value);
      break;

    case OPT_fglobals:
      ffe_set_is_globals (value);
      break;

    case OPT_ffortran_bounds_check:
      flag_bounds_check = value;
      break;

    case OPT_ftypeless_boz:
      ffe_set_is_typeless_boz (value);
      break;

    case OPT_fintrin_case_initcap:
      ffe_set_case_intrin (FFE_caseINITCAP);
      break;

    case OPT_fintrin_case_lower:
      ffe_set_case_intrin (FFE_caseLOWER);
      break;

    case OPT_fintrin_case_upper:
      ffe_set_case_intrin (FFE_caseUPPER);
      break;

    case OPT_fintrin_case_any:
      ffe_set_case_intrin (FFE_caseNONE);
      break;

    case OPT_fmatch_case_initcap:
      ffe_set_case_match (FFE_caseINITCAP);
      break;

    case OPT_fmatch_case_lower:
      ffe_set_case_match (FFE_caseLOWER);
      break;

    case OPT_fmatch_case_upper:
      ffe_set_case_match (FFE_caseUPPER);
      break;

    case OPT_fmatch_case_any:
      ffe_set_case_match (FFE_caseNONE);
      break;

    case OPT_fsource_case_lower:
      ffe_set_case_source (FFE_caseLOWER);
      break;

    case OPT_fsource_case_preserve:
      ffe_set_case_match (FFE_caseNONE);
      break;

    case OPT_fsource_case_upper:
      ffe_set_case_source (FFE_caseUPPER);
      break;

    case OPT_fsymbol_case_initcap:
      ffe_set_case_symbol (FFE_caseINITCAP);
      break;

    case OPT_fsymbol_case_lower:
      ffe_set_case_symbol (FFE_caseLOWER);
      break;

    case OPT_fsymbol_case_upper:
      ffe_set_case_symbol (FFE_caseUPPER);
      break;

    case OPT_fsymbol_case_any:
      ffe_set_case_symbol (FFE_caseNONE);
      break;

    case OPT_fcase_strict_upper:
      ffe_set_case_intrin (FFE_caseUPPER);
      ffe_set_case_match (FFE_caseUPPER);
      ffe_set_case_source (FFE_caseNONE);
      ffe_set_case_symbol (FFE_caseUPPER);
      break;

    case OPT_fcase_strict_lower:
      ffe_set_case_intrin (FFE_caseLOWER);
      ffe_set_case_match (FFE_caseLOWER);
      ffe_set_case_source (FFE_caseNONE);
      ffe_set_case_symbol (FFE_caseLOWER);
      break;

    case OPT_fcase_initcap:
      ffe_set_case_intrin (FFE_caseINITCAP);
      ffe_set_case_match (FFE_caseINITCAP);
      ffe_set_case_source (FFE_caseNONE);
      ffe_set_case_symbol (FFE_caseINITCAP);
      break;

    case OPT_fcase_upper:
      ffe_set_case_intrin (FFE_caseNONE);
      ffe_set_case_match (FFE_caseNONE);
      ffe_set_case_source (FFE_caseUPPER);
      ffe_set_case_symbol (FFE_caseNONE);
      break;

    case OPT_fcase_lower:
      ffe_set_case_intrin (FFE_caseNONE);
      ffe_set_case_match (FFE_caseNONE);
      ffe_set_case_source (FFE_caseLOWER);
      ffe_set_case_symbol (FFE_caseNONE);
      break;

    case OPT_fcase_preserve:
      ffe_set_case_intrin (FFE_caseNONE);
      ffe_set_case_match (FFE_caseNONE);
      ffe_set_case_source (FFE_caseNONE);
      ffe_set_case_symbol (FFE_caseNONE);
      break;

    case OPT_fbadu77_intrinsics_delete:
      ffe_set_intrinsic_state_badu77 (FFE_intrinsicstateDELETED);
      break;

    case OPT_fbadu77_intrinsics_hide:
      ffe_set_intrinsic_state_badu77 (FFE_intrinsicstateHIDDEN);
      break;

    case OPT_fbadu77_intrinsics_disable:
      ffe_set_intrinsic_state_badu77 (FFE_intrinsicstateDISABLED);
      break;

    case OPT_fbadu77_intrinsics_enable:
      ffe_set_intrinsic_state_badu77 (FFE_intrinsicstateENABLED);
      break;

    case OPT_fgnu_intrinsics_delete:
      ffe_set_intrinsic_state_gnu (FFE_intrinsicstateDELETED);
      break;

    case OPT_fgnu_intrinsics_hide:
      ffe_set_intrinsic_state_gnu (FFE_intrinsicstateHIDDEN);
      break;

    case OPT_fgnu_intrinsics_disable:
      ffe_set_intrinsic_state_gnu (FFE_intrinsicstateDISABLED);
      break;

    case OPT_fgnu_intrinsics_enable:
      ffe_set_intrinsic_state_gnu (FFE_intrinsicstateENABLED);
      break;

    case OPT_ff2c_intrinsics_delete:
      ffe_set_intrinsic_state_f2c (FFE_intrinsicstateDELETED);
      break;

    case OPT_ff2c_intrinsics_hide:
      ffe_set_intrinsic_state_f2c (FFE_intrinsicstateHIDDEN);
      break;

    case OPT_ff2c_intrinsics_disable:
      ffe_set_intrinsic_state_f2c (FFE_intrinsicstateDISABLED);
      break;

    case OPT_ff2c_intrinsics_enable:
      ffe_set_intrinsic_state_f2c (FFE_intrinsicstateENABLED);
      break;

    case OPT_ff90_intrinsics_delete:
      ffe_set_intrinsic_state_f90 (FFE_intrinsicstateDELETED);
      break;

    case OPT_ff90_intrinsics_hide:
      ffe_set_intrinsic_state_f90 (FFE_intrinsicstateHIDDEN);
      break;

    case OPT_ff90_intrinsics_disable:
      ffe_set_intrinsic_state_f90 (FFE_intrinsicstateDISABLED);
      break;

    case OPT_ff90_intrinsics_enable:
      ffe_set_intrinsic_state_f90 (FFE_intrinsicstateENABLED);
      break;

    case OPT_fmil_intrinsics_delete:
      ffe_set_intrinsic_state_mil (FFE_intrinsicstateDELETED);
      break;

    case OPT_fmil_intrinsics_hide:
      ffe_set_intrinsic_state_mil (FFE_intrinsicstateHIDDEN);
      break;

    case OPT_fmil_intrinsics_disable:
      ffe_set_intrinsic_state_mil (FFE_intrinsicstateDISABLED);
      break;

    case OPT_fmil_intrinsics_enable:
      ffe_set_intrinsic_state_mil (FFE_intrinsicstateENABLED);
      break;

    case OPT_funix_intrinsics_delete:
      ffe_set_intrinsic_state_unix (FFE_intrinsicstateDELETED);
      break;

    case OPT_funix_intrinsics_hide:
      ffe_set_intrinsic_state_unix (FFE_intrinsicstateHIDDEN);
      break;

    case OPT_funix_intrinsics_disable:
      ffe_set_intrinsic_state_unix (FFE_intrinsicstateDISABLED);
      break;

    case OPT_funix_intrinsics_enable:
      ffe_set_intrinsic_state_unix (FFE_intrinsicstateENABLED);
      break;

    case OPT_fvxt_intrinsics_delete:
      ffe_set_intrinsic_state_vxt (FFE_intrinsicstateDELETED);
      break;

    case OPT_fvxt_intrinsics_hide:
      ffe_set_intrinsic_state_vxt (FFE_intrinsicstateHIDDEN);
      break;

    case OPT_fvxt_intrinsics_disable:
      ffe_set_intrinsic_state_vxt (FFE_intrinsicstateDISABLED);
      break;

    case OPT_fvxt_intrinsics_enable:
      ffe_set_intrinsic_state_vxt (FFE_intrinsicstateENABLED);
      break;

    case OPT_ffixed_line_length_:
      if (strcmp (arg, "none") == 0)
	ffe_set_fixed_line_length (0);
      else if (ffe_is_digit_string_ (arg))
	ffe_set_fixed_line_length (atol (arg));
      else
	return 0;
      break;

    case OPT_Wcomment:
    case OPT_Wcomments:
    case OPT_Wimport:
    case OPT_Wtrigraphs:
    case OPT_fpreprocessed:
      /* These are for cpp.  */
      break;

    case OPT_Wglobals:
      ffe_set_is_warn_globals (value);
      break;

    case OPT_Wimplicit:
      ffe_set_is_warn_implicit (value);
      break;

    case OPT_Wsurprising:
      ffe_set_is_warn_surprising (value);
      break;

    case OPT_Wall:
      set_Wunused (value);
      /* We save the value of warn_uninitialized, since if they put
	 -Wuninitialized on the command line, we need to generate a
	 warning about not using it without also specifying -O.  */
      if (value)
	{
	  if (warn_uninitialized != 1)
	    warn_uninitialized = 2;
	}
      else
	warn_uninitialized = 0;
      break;

    case OPT_I:
      ffecom_decode_include_option (arg);
      break;
    }

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
ffe_init_0 (void)
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
ffe_init_1 (void)
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
ffe_init_2 (void)
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
ffe_init_3 (void)
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
ffe_init_4 (void)
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
ffe_terminate_0 (void)
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
ffe_terminate_1 (void)
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
ffe_terminate_2 (void)
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
ffe_terminate_3 (void)
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
ffe_terminate_4 (void)
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
