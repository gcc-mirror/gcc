/* top.h -- Public #include File (module.h template V1.0)
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

   Owning Modules:
      top.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_TOP_H
#define GCC_F_TOP_H

/* Simple definitions and enumerations. */

enum _ffe_case_
  {
    FFE_caseNONE,		/* No case conversion, match
				   case-insensitive. */
    FFE_caseUPPER,		/* Convert lowercase to uppercase, match
				   upper. */
    FFE_caseLOWER,		/* Convert uppercase to lowercase, match
				   lower. */
    FFE_caseINITCAP,		/* Match InitialCap (no meaning for
				   conversion). */
    FFE_case
  };
typedef enum _ffe_case_ ffeCase;

enum _ffeintrinsic_state_
  {				/* State of a family of intrinsics. NOTE:
				   order IS important, see
				   ffe_intrinsic_state_max (). */
    FFE_intrinsicstateDELETED,	/* Doesn't exist at all. */
    FFE_intrinsicstateDISABLED,	/* Diagnostic if used as intrinsic. */
    FFE_intrinsicstateHIDDEN,	/* Exists only if INTRINSIC stmt. */
    FFE_intrinsicstateENABLED,	/* Exists as normal. */
    FFE_intrinsicstate
  };
typedef enum _ffeintrinsic_state_ ffeIntrinsicState;

/* Typedefs. */

typedef unsigned long ffeCounter;
#define ffeCounter_f "l"
typedef unsigned int ffeKwIndex;
typedef unsigned long int ffeTokenLength;
#define ffeTokenLength_f "l"
typedef void *ffeUnionLongPtr;	/* unused type to cover union of long and
				   ptr. */

/* Include files needed by this one. */

#include "malloc.h"
#include "where.h"

/* Structure definitions. */


/* Global objects accessed by users of this module. */

extern bool ffe_is_do_internal_checks_;
extern bool ffe_is_90_;
extern bool ffe_is_automatic_;
extern bool ffe_is_backslash_;
extern bool ffe_is_emulate_complex_;
extern bool ffe_is_underscoring_;
extern bool ffe_is_second_underscore_;
extern bool ffe_is_debug_kludge_;
extern bool ffe_is_dollar_ok_;
extern bool ffe_is_f2c_;
extern bool ffe_is_f2c_library_;
extern bool ffe_is_ffedebug_;
extern bool ffe_is_flatten_arrays_;
extern bool ffe_is_free_form_;
extern bool ffe_is_globals_;
extern bool ffe_is_init_local_zero_;
extern bool ffe_is_mainprog_;
extern bool ffe_is_onetrip_;
extern bool ffe_is_silent_;
extern bool ffe_is_typeless_boz_;
extern bool ffe_is_pedantic_;
extern bool ffe_is_saveall_;
extern bool ffe_is_ugly_args_;
extern bool ffe_is_ugly_assign_;
extern bool ffe_is_ugly_assumed_;
extern bool ffe_is_ugly_comma_;
extern bool ffe_is_ugly_complex_;
extern bool ffe_is_ugly_init_;
extern bool ffe_is_ugly_logint_;
extern bool ffe_is_version_;
extern bool ffe_is_vxt_;
extern bool ffe_is_warn_globals_;
extern bool ffe_is_warn_implicit_;
extern bool ffe_is_warn_surprising_;
extern bool ffe_is_zeros_;
extern ffeCase ffe_case_intrin_;
extern ffeCase ffe_case_match_;
extern ffeCase ffe_case_source_;
extern ffeCase ffe_case_symbol_;
extern ffeIntrinsicState ffe_intrinsic_state_badu77_;
extern ffeIntrinsicState ffe_intrinsic_state_gnu_;
extern ffeIntrinsicState ffe_intrinsic_state_f2c_;
extern ffeIntrinsicState ffe_intrinsic_state_f90_;
extern ffeIntrinsicState ffe_intrinsic_state_mil_;
extern ffeIntrinsicState ffe_intrinsic_state_unix_;
extern ffeIntrinsicState ffe_intrinsic_state_vxt_;
extern int ffe_fixed_line_length_;
extern mallocPool ffe_file_pool_;
extern mallocPool ffe_any_unit_pool_;
extern mallocPool ffe_program_unit_pool_;
extern ffeCounter ffe_count_0;
extern ffeCounter ffe_count_1;
extern ffeCounter ffe_count_2;
extern ffeCounter ffe_count_3;
extern ffeCounter ffe_count_4;
extern bool ffe_in_0;
extern bool ffe_in_1;
extern bool ffe_in_2;
extern bool ffe_in_3;
extern bool ffe_in_4;

/* Declare functions with prototypes. */

int ffe_decode_option (int argc, char **argv);
void ffe_file (ffewhereFile wf, FILE *f);
void ffe_init_0 (void);
void ffe_init_1 (void);
void ffe_init_2 (void);
void ffe_init_3 (void);
void ffe_init_4 (void);
void ffe_terminate_0 (void);
void ffe_terminate_1 (void);
void ffe_terminate_2 (void);
void ffe_terminate_3 (void);
void ffe_terminate_4 (void);

/* Define macros. */

#define ffe_case_intrin() ffe_case_intrin_
#define ffe_case_match() ffe_case_match_
#define ffe_case_source() ffe_case_source_
#define ffe_case_symbol() ffe_case_symbol_
#define ffe_intrinsic_state_badu77() ffe_intrinsic_state_badu77_
#define ffe_intrinsic_state_f2c() ffe_intrinsic_state_f2c_
#define ffe_intrinsic_state_f90() ffe_intrinsic_state_f90_
#define ffe_intrinsic_state_gnu() ffe_intrinsic_state_gnu_
#define ffe_intrinsic_state_mil() ffe_intrinsic_state_mil_
#define ffe_intrinsic_state_unix() ffe_intrinsic_state_unix_
#define ffe_intrinsic_state_vxt() ffe_intrinsic_state_vxt_
#define ffe_is_90() ffe_is_90_
#define ffe_is_automatic() ffe_is_automatic_
#define ffe_is_backslash() ffe_is_backslash_
#define ffe_is_debug_kludge() ffe_is_debug_kludge_
#define ffe_is_do_internal_checks() ffe_is_do_internal_checks_
#define ffe_is_dollar_ok() ffe_is_dollar_ok_
#define ffe_is_emulate_complex() ffe_is_emulate_complex_
#define ffe_is_f2c() ffe_is_f2c_
#define ffe_is_f2c_library() ffe_is_f2c_library_
#define ffe_is_ffedebug() ffe_is_ffedebug_
#define ffe_is_flatten_arrays() ffe_is_flatten_arrays_
#define ffe_is_free_form() ffe_is_free_form_
#define ffe_is_globals() ffe_is_globals_
#define ffe_is_init_local_zero() ffe_is_init_local_zero_
#define ffe_is_mainprog() ffe_is_mainprog_
#define ffe_is_onetrip() ffe_is_onetrip_
#define ffe_is_pedantic() ffe_is_pedantic_
#define ffe_is_pedantic_not_90() (ffe_is_pedantic_ && !ffe_is_90_)
#define ffe_is_saveall() ffe_is_saveall_
#define ffe_is_second_underscore() ffe_is_second_underscore_
#define ffe_is_silent() ffe_is_silent_
#define ffe_is_typeless_boz() ffe_is_typeless_boz_
#define ffe_is_ugly_args() ffe_is_ugly_args_
#define ffe_is_ugly_assign() ffe_is_ugly_assign_
#define ffe_is_ugly_assumed() ffe_is_ugly_assumed_
#define ffe_is_ugly_comma() ffe_is_ugly_comma_
#define ffe_is_ugly_complex() ffe_is_ugly_complex_
#define ffe_is_ugly_init() ffe_is_ugly_init_
#define ffe_is_ugly_logint() ffe_is_ugly_logint_
#define ffe_is_underscoring() ffe_is_underscoring_
#define ffe_is_version() ffe_is_version_
#define ffe_is_vxt() ffe_is_vxt_
#define ffe_is_warn_globals() ffe_is_warn_globals_
#define ffe_is_warn_implicit() ffe_is_warn_implicit_
#define ffe_is_warn_surprising() ffe_is_warn_surprising_
#define ffe_is_zeros() ffe_is_zeros_
#define ffe_fixed_line_length() ffe_fixed_line_length_
#define ffe_pool_file() (ffe_file_pool_)
#define ffe_pool_any_unit() (ffe_any_unit_pool_)
#define ffe_pool_program_unit() (ffe_program_unit_pool_)
#define ffe_set_case_intrin(f) (ffe_case_intrin_ = (f))
#define ffe_set_case_match(f) (ffe_case_match_ = (f))
#define ffe_set_case_source(f) (ffe_case_source_ = (f))
#define ffe_set_case_symbol(f) (ffe_case_symbol_ = (f))
#define ffe_set_intrinsic_state_badu77(s) (ffe_intrinsic_state_badu77_ = (s))
#define ffe_set_intrinsic_state_f2c(s) (ffe_intrinsic_state_f2c_ = (s))
#define ffe_set_intrinsic_state_f90(s) (ffe_intrinsic_state_f90_ = (s))
#define ffe_set_intrinsic_state_gnu(s) (ffe_intrinsic_state_gnu_ = (s))
#define ffe_set_intrinsic_state_mil(s) (ffe_intrinsic_state_mil_ = (s))
#define ffe_set_intrinsic_state_unix(s) (ffe_intrinsic_state_unix_ = (s))
#define ffe_set_intrinsic_state_vxt(s) (ffe_intrinsic_state_vxt_ = (s))
#define ffe_set_is_90(f) (ffe_is_90_ = (f))
#define ffe_set_is_automatic(f) (ffe_is_automatic_ = (f))
#define ffe_set_is_backslash(f) (ffe_is_backslash_ = (f))
#define ffe_set_is_debug_kludge(f) (ffe_is_debug_kludge_ = (f))
#define ffe_set_is_do_internal_checks(f) (ffe_is_do_internal_checks_ = (f))
#define ffe_set_is_dollar_ok(f) (ffe_is_dollar_ok_ = (f))
#define ffe_set_is_emulate_complex(f) (ffe_is_emulate_complex_ = (f))
#define ffe_set_is_f2c(f) (ffe_is_f2c_ = (f))
#define ffe_set_is_f2c_library(f) (ffe_is_f2c_library_ = (f))
#define ffe_set_is_ffedebug(f) (ffe_is_ffedebug_ = (f))
#define ffe_set_is_flatten_arrays(f) (ffe_is_flatten_arrays_ = (f))
#define ffe_set_is_free_form(f) (ffe_is_free_form_ = (f))
#define ffe_set_is_globals(f) (ffe_is_globals_ = (f))
#define ffe_set_is_init_local_zero(f) (ffe_is_init_local_zero_ = (f))
#define ffe_set_is_mainprog(f) (ffe_is_mainprog_ = (f))
#define ffe_set_is_onetrip(f) (ffe_is_onetrip_ = (f))
#define ffe_set_is_pedantic(f) (ffe_is_pedantic_ = (f))
#define ffe_set_is_saveall(f) (ffe_is_saveall_ = (f))
#define ffe_set_is_second_underscore(f) (ffe_is_second_underscore_ = (f))
#define ffe_set_is_silent(f) (ffe_is_silent_ = (f))
#define ffe_set_is_typeless_boz(f) (ffe_is_typeless_boz_ = (f))
#define ffe_set_is_ugly_args(f) (ffe_is_ugly_args_ = (f))
#define ffe_set_is_ugly_assign(f) (ffe_is_ugly_assign_ = (f))
#define ffe_set_is_ugly_assumed(f) (ffe_is_ugly_assumed_ = (f))
#define ffe_set_is_ugly_comma(f) (ffe_is_ugly_comma_ = (f))
#define ffe_set_is_ugly_complex(f) (ffe_is_ugly_complex_ = (f))
#define ffe_set_is_ugly_init(f) (ffe_is_ugly_init_ = (f))
#define ffe_set_is_ugly_logint(f) (ffe_is_ugly_logint_ = (f))
#define ffe_set_is_underscoring(f) (ffe_is_underscoring_ = (f))
#define ffe_set_is_version(f) (ffe_is_version_ = (f))
#define ffe_set_is_vxt(f) (ffe_is_vxt_ = (f))
#define ffe_set_is_warn_globals(f) (ffe_is_warn_globals_ = (f))
#define ffe_set_is_warn_implicit(f) (ffe_is_warn_implicit_ = (f))
#define ffe_set_is_warn_surprising(f) (ffe_is_warn_surprising_ = (f))
#define ffe_set_is_zeros(f) (ffe_is_zeros_ = (f))
#define ffe_set_fixed_line_length(l) (ffe_fixed_line_length_ = (l))
#define ffe_state_max(s1,s2) ((s1) > (s2) ? (s1) : (s2))

/* End of #include file. */

#endif /* ! GCC_F_TOP_H */
