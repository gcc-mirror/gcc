/* lang-options.h file for Fortran
   Copyright (C) 1995-1998 Free Software Foundation, Inc.
   Contributed by James Craig Burley (burley@gnu.org).

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

DEFINE_LANG_NAME ("Fortran")
     
  { "-fversion", "Print g77-specific compiler version info, run internal tests" },
  { "-fnull-version", "" },
/*"-fident",*/
/*"-fno-ident",*/
  { "-ff66", "Program is written in typical FORTRAN 66 dialect" },
  { "-fno-f66", "" },
  { "-ff77", "Program is written in typical Unix f77 dialect" },
  { "-fno-f77", "Program does not use Unix-f77 dialectal features" },
  { "-ff90", "Program is written in Fortran-90-ish dialect" },
  { "-fno-f90", "" },
  { "-fautomatic", "" },
  { "-fno-automatic", "Treat local vars and COMMON blocks as if they were named in SAVE statements" },
  { "-fdollar-ok", "Allow $ in symbol names" },
  { "-fno-dollar-ok", "" },
  { "-ff2c", "" },
  { "-fno-f2c", "f2c-compatible code need not be generated" },
  { "-ff2c-library", "" },
  { "-fno-f2c-library", "Unsupported; do not generate libf2c-calling code" },
  { "-ffree-form", "Program is written in Fortran-90-ish free form" },
  { "-fno-free-form", "" },
  { "-ffixed-form", "" },
  { "-fno-fixed-form", "" },
  { "-fpedantic", "Warn about use of (only a few for now) Fortran extensions" },
  { "-fno-pedantic", "" },
  { "-fvxt", "Program is written in VXT (Digital-like) FORTRAN" },
  { "-fno-vxt", "" },
  { "-fugly", "Obsolete; allow certain ugly features" },
  { "-fno-ugly", "" },
  { "-fugly-args", "" },
  { "-fno-ugly-args", "Hollerith and typeless constants not passed as arguments" },
  { "-fugly-assign", "Allow ordinary copying of ASSIGN'ed vars" },
  { "-fno-ugly-assign", "" },
  { "-fugly-assumed", "Dummy array dimensioned to (1) is assumed-size" },
  { "-fno-ugly-assumed", "" },
  { "-fugly-comma", "Trailing comma in procedure call denotes null argument" },
  { "-fno-ugly-comma", "" },
  { "-fugly-complex", "Allow REAL(Z) and AIMAG(Z) given DOUBLE COMPLEX Z" },
  { "-fno-ugly-complex", "" },
  { "-fugly-init", "" },
  { "-fno-ugly-init", "Initialization via DATA and PARAMETER is type-compatible" },
  { "-fugly-logint", "Allow INTEGER and LOGICAL interchangeability" },
  { "-fno-ugly-logint", "" },
  { "-fxyzzy", "Print internal debugging-related info" },
  { "-fno-xyzzy", "" },
  { "-finit-local-zero", "Initialize local vars and arrays to zero" },
  { "-fno-init-local-zero", "" },
  { "-fbackslash", "" },
  { "-fno-backslash", "Backslashes in character/hollerith constants not special (C-style)" },
  { "-femulate-complex", "" },
  { "-fno-emulate-complex", "Have compiler back end cope with COMPLEX arithmetic" },
  { "-funderscoring", "" },
  { "-fno-underscoring", "Disable the appending of underscores to externals" },
  { "-fsecond-underscore", "" },
  { "-fno-second-underscore", "Never append a second underscore to externals" },
  { "-fintrin-case-initcap", "Intrinsics spelled as e.g. SqRt" },
  { "-fintrin-case-upper", "Intrinsics in uppercase" },
  { "-fintrin-case-lower", "" },
  { "-fintrin-case-any", "Intrinsics letters in arbitrary cases" },
  { "-fmatch-case-initcap", "Language keywords spelled as e.g. IOStat" },
  { "-fmatch-case-upper", "Language keywords in uppercase" },
  { "-fmatch-case-lower", "" },
  { "-fmatch-case-any", "Language keyword letters in arbitrary cases" },
  { "-fsource-case-upper", "Internally convert most source to uppercase" },
  { "-fsource-case-lower", "" },
  { "-fsource-case-preserve", "Internally preserve source case" },
  { "-fsymbol-case-initcap", "Symbol names spelled in mixed case" },
  { "-fsymbol-case-upper", "Symbol names in uppercase" },
  { "-fsymbol-case-lower", "Symbol names in lowercase" },
  { "-fsymbol-case-any", "" },
  { "-fcase-strict-upper", "Program written in uppercase" },
  { "-fcase-strict-lower", "Program written in lowercase" },
  { "-fcase-initcap", "Program written in strict mixed-case" },
  { "-fcase-upper", "Compile as if program written in uppercase" },
  { "-fcase-lower", "Compile as if program written in lowercase" },
  { "-fcase-preserve", "Preserve all spelling (case) used in program" },
  { "-fbadu77-intrinsics-delete", "Delete libU77 intrinsics with bad interfaces" },
  { "-fbadu77-intrinsics-disable", "Disable libU77 intrinsics with bad interfaces" },
  { "-fbadu77-intrinsics-enable", "" },
  { "-fbadu77-intrinsics-hide", "Hide libU77 intrinsics with bad interfaces" },
  { "-ff2c-intrinsics-delete", "Delete non-FORTRAN-77 intrinsics f2c supports" },
  { "-ff2c-intrinsics-disable", "Disable non-FORTRAN-77 intrinsics f2c supports" },
  { "-ff2c-intrinsics-enable", "" },
  { "-ff2c-intrinsics-hide", "Hide non-FORTRAN-77 intrinsics f2c supports" },
  { "-ff90-intrinsics-delete", "Delete non-FORTRAN-77 intrinsics F90 supports" },
  { "-ff90-intrinsics-disable", "Disable non-FORTRAN-77 intrinsics F90 supports" },
  { "-ff90-intrinsics-enable", "" },
  { "-ff90-intrinsics-hide", "Hide non-FORTRAN-77 intrinsics F90 supports" },
  { "-fgnu-intrinsics-delete", "Delete non-FORTRAN-77 intrinsics g77 supports" },
  { "-fgnu-intrinsics-disable", "Disable non-FORTRAN 77 intrinsics F90 supports" },
  { "-fgnu-intrinsics-enable", "" },
  { "-fgnu-intrinsics-hide", "Hide non-FORTRAN 77 intrinsics F90 supports" },
  { "-fmil-intrinsics-delete", "Delete MIL-STD 1753 intrinsics" },
  { "-fmil-intrinsics-disable", "Disable MIL-STD 1753 intrinsics" },
  { "-fmil-intrinsics-enable", "" },
  { "-fmil-intrinsics-hide", "Hide MIL-STD 1753 intrinsics" },
  { "-funix-intrinsics-delete", "Delete libU77 intrinsics" },
  { "-funix-intrinsics-disable", "Disable libU77 intrinsics" },
  { "-funix-intrinsics-enable", "" },
  { "-funix-intrinsics-hide", "Hide libU77 intrinsics" },
  { "-fvxt-intrinsics-delete", "Delete non-FORTRAN-77 intrinsics VXT FORTRAN supports" },
  { "-fvxt-intrinsics-disable", "Disable non-FORTRAN-77 intrinsics VXT FORTRAN supports" },
  { "-fvxt-intrinsics-enable", "" },
  { "-fvxt-intrinsics-hide", "Hide non-FORTRAN-77 intrinsics VXT FORTRAN supports" },
  { "-fzeros", "Treat initial values of 0 like non-zero values" },
  { "-fno-zeros", "" },
  { "-fdebug-kludge", "Emit special debugging information for COMMON and EQUIVALENCE" },
  { "-fno-debug-kludge", "" },
  { "-fonetrip", "Take at least one trip through each iterative DO loop" },
  { "-fno-onetrip", "" },
  { "-fsilent", "" },
  { "-fno-silent", "Print names of program units as they are compiled" },
  { "-fglobals", "" },
  { "-fno-globals", "Disable fatal diagnostics about inter-procedural problems" },
  { "-ftypeless-boz", "Make prefix-radix non-decimal constants be typeless" },
  { "-fno-typeless-boz", "" },
  { "-Wglobals", "" },
  { "-Wno-globals", "Disable warnings about inter-procedural problems" },
/*"-Wimplicit",*/
/*"-Wno-implicit",*/
  { "-Wsurprising", "Warn about constructs with surprising meanings" },
  { "-Wno-surprising", "" },
/*"-Wall",*/
/* Prefix options.  */
  { "-I", "Add a directory for INCLUDE searching" },
  { "-ffixed-line-length-", "Set the maximum line length" },

#endif
