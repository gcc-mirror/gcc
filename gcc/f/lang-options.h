/* lang-options.h file for Fortran
   Copyright (C) 1995-1998 Free Software Foundation, Inc.
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

*/

/* This is the contribution to the `lang_options' array in gcc.c for
   g77.  */

#ifdef __STDC__	/* To be consistent with lang-specs.h.  Maybe avoid
		   overflowing some old compiler's tables, etc. */

DEFINE_LANG_NAME ("Fortran")

/* Use of FTNOPT makes tracking changes between FSF-g77 and egcs-g77
   easier, since FSF-gcc doesn't support doc strings.  */
#define FTNOPT(opt,doc) { opt, doc },

FTNOPT( "-fversion", "Print g77-specific compiler version info, run internal tests" )
FTNOPT( "-fnull-version", "" )
/*"-fident",*/
/*"-fno-ident",*/
FTNOPT( "-ff66", "Program is written in typical FORTRAN 66 dialect" )
FTNOPT( "-fno-f66", "" )
FTNOPT( "-ff77", "Program is written in typical Unix f77 dialect" )
FTNOPT( "-fno-f77", "Program does not use Unix-f77 dialectal features" )
FTNOPT( "-ff90", "Program is written in Fortran-90-ish dialect" )
FTNOPT( "-fno-f90", "" )
FTNOPT( "-fautomatic", "" )
FTNOPT( "-fno-automatic", "Treat local vars and COMMON blocks as if they were named in SAVE statements" )
FTNOPT( "-fdollar-ok", "Allow $ in symbol names" )
FTNOPT( "-fno-dollar-ok", "" )
FTNOPT( "-ff2c", "" )
FTNOPT( "-fno-f2c", "f2c-compatible code need not be generated" )
FTNOPT( "-ff2c-library", "" )
FTNOPT( "-fno-f2c-library", "Unsupported; do not generate libf2c-calling code" )
FTNOPT( "-fflatten-arrays", "Unsupported; affects code-generation of arrays" )
FTNOPT( "-fno-flatten-arrays", "" )
FTNOPT( "-ffree-form", "Program is written in Fortran-90-ish free form" )
FTNOPT( "-fno-free-form", "" )
FTNOPT( "-ffixed-form", "" )
FTNOPT( "-fno-fixed-form", "" )
FTNOPT( "-fpedantic", "Warn about use of (only a few for now) Fortran extensions" )
FTNOPT( "-fno-pedantic", "" )
FTNOPT( "-fvxt", "Program is written in VXT (Digital-like) FORTRAN" )
FTNOPT( "-fno-vxt", "" )
FTNOPT( "-fno-ugly", "Disallow all ugly features" )
FTNOPT( "-fugly-args", "" )
FTNOPT( "-fno-ugly-args", "Hollerith and typeless constants not passed as arguments" )
FTNOPT( "-fugly-assign", "Allow ordinary copying of ASSIGN'ed vars" )
FTNOPT( "-fno-ugly-assign", "" )
FTNOPT( "-fugly-assumed", "Dummy array dimensioned to (1) is assumed-size" )
FTNOPT( "-fno-ugly-assumed", "" )
FTNOPT( "-fugly-comma", "Trailing comma in procedure call denotes null argument" )
FTNOPT( "-fno-ugly-comma", "" )
FTNOPT( "-fugly-complex", "Allow REAL(Z) and AIMAG(Z) given DOUBLE COMPLEX Z" )
FTNOPT( "-fno-ugly-complex", "" )
FTNOPT( "-fugly-init", "" )
FTNOPT( "-fno-ugly-init", "Initialization via DATA and PARAMETER is type-compatible" )
FTNOPT( "-fugly-logint", "Allow INTEGER and LOGICAL interchangeability" )
FTNOPT( "-fno-ugly-logint", "" )
FTNOPT( "-fxyzzy", "Print internal debugging-related info" )
FTNOPT( "-fno-xyzzy", "" )
FTNOPT( "-finit-local-zero", "Initialize local vars and arrays to zero" )
FTNOPT( "-fno-init-local-zero", "" )
FTNOPT( "-fbackslash", "" )
FTNOPT( "-fno-backslash", "Backslashes in character/hollerith constants not special (C-style)" )
FTNOPT( "-femulate-complex", "Have front end emulate COMPLEX arithmetic to avoid bugs" )
FTNOPT( "-fno-emulate-complex", "" )
FTNOPT( "-funderscoring", "" )
FTNOPT( "-fno-underscoring", "Disable the appending of underscores to externals" )
FTNOPT( "-fsecond-underscore", "" )
FTNOPT( "-fno-second-underscore", "Never append a second underscore to externals" )
FTNOPT( "-fintrin-case-initcap", "Intrinsics spelled as e.g. SqRt" )
FTNOPT( "-fintrin-case-upper", "Intrinsics in uppercase" )
FTNOPT( "-fintrin-case-lower", "" )
FTNOPT( "-fintrin-case-any", "Intrinsics letters in arbitrary cases" )
FTNOPT( "-fmatch-case-initcap", "Language keywords spelled as e.g. IOStat" )
FTNOPT( "-fmatch-case-upper", "Language keywords in uppercase" )
FTNOPT( "-fmatch-case-lower", "" )
FTNOPT( "-fmatch-case-any", "Language keyword letters in arbitrary cases" )
FTNOPT( "-fsource-case-upper", "Internally convert most source to uppercase" )
FTNOPT( "-fsource-case-lower", "" )
FTNOPT( "-fsource-case-preserve", "Internally preserve source case" )
FTNOPT( "-fsymbol-case-initcap", "Symbol names spelled in mixed case" )
FTNOPT( "-fsymbol-case-upper", "Symbol names in uppercase" )
FTNOPT( "-fsymbol-case-lower", "Symbol names in lowercase" )
FTNOPT( "-fsymbol-case-any", "" )
FTNOPT( "-fcase-strict-upper", "Program written in uppercase" )
FTNOPT( "-fcase-strict-lower", "Program written in lowercase" )
FTNOPT( "-fcase-initcap", "Program written in strict mixed-case" )
FTNOPT( "-fcase-upper", "Compile as if program written in uppercase" )
FTNOPT( "-fcase-lower", "Compile as if program written in lowercase" )
FTNOPT( "-fcase-preserve", "Preserve all spelling (case) used in program" )
FTNOPT( "-fbadu77-intrinsics-delete", "Delete libU77 intrinsics with bad interfaces" )
FTNOPT( "-fbadu77-intrinsics-disable", "Disable libU77 intrinsics with bad interfaces" )
FTNOPT( "-fbadu77-intrinsics-enable", "" )
FTNOPT( "-fbadu77-intrinsics-hide", "Hide libU77 intrinsics with bad interfaces" )
FTNOPT( "-ff2c-intrinsics-delete", "Delete non-FORTRAN-77 intrinsics f2c supports" )
FTNOPT( "-ff2c-intrinsics-disable", "Disable non-FORTRAN-77 intrinsics f2c supports" )
FTNOPT( "-ff2c-intrinsics-enable", "" )
FTNOPT( "-ff2c-intrinsics-hide", "Hide non-FORTRAN-77 intrinsics f2c supports" )
FTNOPT( "-ff90-intrinsics-delete", "Delete non-FORTRAN-77 intrinsics F90 supports" )
FTNOPT( "-ff90-intrinsics-disable", "Disable non-FORTRAN-77 intrinsics F90 supports" )
FTNOPT( "-ff90-intrinsics-enable", "" )
FTNOPT( "-ff90-intrinsics-hide", "Hide non-FORTRAN-77 intrinsics F90 supports" )
FTNOPT( "-fgnu-intrinsics-delete", "Delete non-FORTRAN-77 intrinsics g77 supports" )
FTNOPT( "-fgnu-intrinsics-disable", "Disable non-FORTRAN 77 intrinsics F90 supports" )
FTNOPT( "-fgnu-intrinsics-enable", "" )
FTNOPT( "-fgnu-intrinsics-hide", "Hide non-FORTRAN 77 intrinsics F90 supports" )
FTNOPT( "-fmil-intrinsics-delete", "Delete MIL-STD 1753 intrinsics" )
FTNOPT( "-fmil-intrinsics-disable", "Disable MIL-STD 1753 intrinsics" )
FTNOPT( "-fmil-intrinsics-enable", "" )
FTNOPT( "-fmil-intrinsics-hide", "Hide MIL-STD 1753 intrinsics" )
FTNOPT( "-funix-intrinsics-delete", "Delete libU77 intrinsics" )
FTNOPT( "-funix-intrinsics-disable", "Disable libU77 intrinsics" )
FTNOPT( "-funix-intrinsics-enable", "" )
FTNOPT( "-funix-intrinsics-hide", "Hide libU77 intrinsics" )
FTNOPT( "-fvxt-intrinsics-delete", "Delete non-FORTRAN-77 intrinsics VXT FORTRAN supports" )
FTNOPT( "-fvxt-intrinsics-disable", "Disable non-FORTRAN-77 intrinsics VXT FORTRAN supports" )
FTNOPT( "-fvxt-intrinsics-enable", "" )
FTNOPT( "-fvxt-intrinsics-hide", "Hide non-FORTRAN-77 intrinsics VXT FORTRAN supports" )
FTNOPT( "-fzeros", "Treat initial values of 0 like non-zero values" )
FTNOPT( "-fno-zeros", "" )
FTNOPT( "-fdebug-kludge", "Emit special debugging information for COMMON and EQUIVALENCE" )
FTNOPT( "-fno-debug-kludge", "" )
FTNOPT( "-fonetrip", "Take at least one trip through each iterative DO loop" )
FTNOPT( "-fno-onetrip", "" )
FTNOPT( "-fsilent", "" )
FTNOPT( "-fno-silent", "Print names of program units as they are compiled" )
FTNOPT( "-fglobals", "" )
FTNOPT( "-fno-globals", "Disable fatal diagnostics about inter-procedural problems" )
FTNOPT( "-ftypeless-boz", "Make prefix-radix non-decimal constants be typeless" )
FTNOPT( "-fno-typeless-boz", "" )
FTNOPT( "-fbounds-check", "Generate code to check subscript and substring bounds" )
FTNOPT( "-fno-bounds-check", "" )
FTNOPT( "-ffortran-bounds-check", "Fortran-specific form of -fbounds-check")
FTNOPT( "-fno-fortran-bounds-check", "" )
FTNOPT( "-Wglobals", "" )
FTNOPT( "-Wno-globals", "Disable warnings about inter-procedural problems" )
/*"-Wimplicit",*/
/*"-Wno-implicit",*/
FTNOPT( "-Wsurprising", "Warn about constructs with surprising meanings" )
FTNOPT( "-Wno-surprising", "" )
/*"-Wall",*/
/* Prefix options.  */
FTNOPT( "-I", "Add a directory for INCLUDE searching" )
FTNOPT( "-ffixed-line-length-", "Set the maximum line length" )

#undef FTNOPT

#endif
