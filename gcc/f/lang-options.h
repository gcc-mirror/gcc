/* lang-options.h file for Fortran
   Copyright (C) 1995-1998 Free Software Foundation, Inc.
   Contributed by James Craig Burley (burley@gnu.ai.mit.edu).

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

*/

/* This is the contribution to the `lang_options' array in gcc.c for
   g77.  */

#ifdef __STDC__	/* To be consistent with lang-specs.h.  Maybe avoid
		   overflowing some old compiler's tables, etc. */

  "-fversion",
  "-fnull-version",
  "-fset-g77-defaults",
/*"-fident",*/
/*"-fno-ident",*/
  "-ff66",
  "-fno-f66",
  "-ff77",
  "-fno-f77",
  "-ff90",
  "-fno-f90",
  "-fautomatic",
  "-fno-automatic",
  "-fdollar-ok",
  "-fno-dollar-ok",
  "-ff2c",
  "-fno-f2c",
  "-ff2c-library",
  "-fno-f2c-library",
  "-ffree-form",
  "-fno-free-form",
  "-ffixed-form",
  "-fno-fixed-form",
  "-fpedantic",
  "-fno-pedantic",
  "-fvxt",
  "-fno-vxt",
  "-fugly",
  "-fno-ugly",
  "-fugly-args",
  "-fno-ugly-args",
  "-fugly-assign",
  "-fno-ugly-assign",
  "-fugly-assumed",
  "-fno-ugly-assumed",
  "-fugly-comma",
  "-fno-ugly-comma",
  "-fugly-complex",
  "-fno-ugly-complex",
  "-fugly-init",
  "-fno-ugly-init",
  "-fugly-logint",
  "-fno-ugly-logint",
  "-fxyzzy",
  "-fno-xyzzy",
  "-finit-local-zero",
  "-fno-init-local-zero",
  "-fbackslash",
  "-fno-backslash",
  "-femulate-complex",
  "-fno-emulate-complex",
  "-funderscoring",
  "-fno-underscoring",
  "-fsecond-underscore",
  "-fno-second-underscore",
  "-fintrin-case-initcap",
  "-fintrin-case-upper",
  "-fintrin-case-lower",
  "-fintrin-case-any",
  "-fmatch-case-initcap",
  "-fmatch-case-upper",
  "-fmatch-case-lower",
  "-fmatch-case-any",
  "-fsource-case-upper",
  "-fsource-case-lower",
  "-fsource-case-preserve",
  "-fsymbol-case-initcap",
  "-fsymbol-case-upper",
  "-fsymbol-case-lower",
  "-fsymbol-case-any",
  "-fcase-strict-upper",
  "-fcase-strict-lower",
  "-fcase-initcap",
  "-fcase-upper",
  "-fcase-lower",
  "-fcase-preserve",
  "-fdcp-intrinsics-delete",
  "-fdcp-intrinsics-hide",
  "-fdcp-intrinsics-disable",
  "-fdcp-intrinsics-enable",
  "-ff2c-intrinsics-delete",
  "-ff2c-intrinsics-hide",
  "-ff2c-intrinsics-disable",
  "-ff2c-intrinsics-enable",
  "-fgnu-intrinsics-delete",
  "-fgnu-intrinsics-hide",
  "-fgnu-intrinsics-disable",
  "-fgnu-intrinsics-enable",
  "-fbadu77-intrinsics-delete",
  "-fbadu77-intrinsics-hide",
  "-fbadu77-intrinsics-disable",
  "-fbadu77-intrinsics-enable",
  "-ff90-intrinsics-delete",
  "-ff90-intrinsics-hide",
  "-ff90-intrinsics-disable",
  "-ff90-intrinsics-enable",
  "-fmil-intrinsics-delete",
  "-fmil-intrinsics-hide",
  "-fmil-intrinsics-disable",
  "-fmil-intrinsics-enable",
  "-funix-intrinsics-delete",
  "-funix-intrinsics-hide",
  "-funix-intrinsics-disable",
  "-funix-intrinsics-enable",
  "-fvxt-intrinsics-delete",
  "-fvxt-intrinsics-hide",
  "-fvxt-intrinsics-disable",
  "-fvxt-intrinsics-enable",
  "-fzeros",
  "-fno-zeros",
  "-fdebug-kludge",
  "-fno-debug-kludge",
  "-fonetrip",
  "-fno-onetrip",
  "-fsilent",
  "-fno-silent",
  "-fglobals",
  "-fno-globals",
  "-ftypeless-boz",
  "-fno-typeless-boz",
  "-Wglobals",
  "-Wno-globals",
/*"-Wimplicit",*/
/*"-Wno-implicit",*/
  "-Wsurprising",
  "-Wno-surprising",
/*"-Wall",*/
/* Prefix options.  */
  "-I",
  "-ffixed-line-length-",
#endif
