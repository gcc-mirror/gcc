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
     
  { "-fversion", "Report running the g77 specific compiler phase" },
  { "-fnull-version", "" },
/*"-fident",*/
/*"-fno-ident",*/
  { "-ff66", "Program is written in FORTRAN66" },
  { "-fno-f66", "" },
  { "-ff77", "Program is written in FORTRAN77" },
  { "-fno-f77", "" },
  { "-ff90", "Program is written in FORTRAN-90" },
  { "-fno-f90", "" },
  { "-fautomatic", "" },
  { "-fno-automatic", "Treat locals as if they were named in SAVE statements" },
  { "-fdollar-ok", "Allow $ in symbol names" },
  { "-fno-dollar-ok", "" },
  { "-ff2c", "" },
  { "-fno-f2c", "Do not generate code compatible with f2c" },
  { "-ff2c-library", "" },
  { "-fno-f2c-library", "" },
  { "-ffree-form", "" },
  { "-fno-free-form", "" },
  { "-ffixed-form", "" },
  { "-fno-fixed-form", "source is written in free form" },
  { "-fpedantic", "Warn about use of Fortran extensions" },
  { "-fno-pedantic", "" },
  { "-fvxt", "Program is written in VXT FORTRAN" },
  { "-fno-vxt", "" },
  { "-fugly", "Allow certain ugly features" },
  { "-fno-ugly", "" },
  { "-fugly-args", },
  { "-fno-ugly-args", },
  { "-fugly-assign", },
  { "-fno-ugly-assign", },
  { "-fugly-assumed", },
  { "-fno-ugly-assumed", },
  { "-fugly-comma", },
  { "-fno-ugly-comma", },
  { "-fugly-complex", },
  { "-fno-ugly-complex", },
  { "-fugly-init", },
  { "-fno-ugly-init", },
  { "-fugly-logint", },
  { "-fno-ugly-logint", },
  { "-fxyzzy", "Enable debugging of Fortran front end" },
  { "-fno-xyzzy", "" },
  { "-finit-local-zero", "Initialise local arrays to zero" },
  { "-fno-init-local-zero", "" },
  { "-fbackslash", "" },
  { "-fno-backslash", "Do not interpret \\ preceeded characters specially" },
  { "-femulate-complex", "" },
  { "-fno-emulate-complex", "Do not attempt to emulate COMPLEX arithmetic" },
  { "-funderscoring", "" },
  { "-fno-underscoring", "Disable the prepending of underscores to externals" },
  { "-fsecond-underscore", "Do not append a second underscore to externals" },
  { "-fno-second-underscore", "" },
  { "-fintrin-case-initcap", },
  { "-fintrin-case-upper", },
  { "-fintrin-case-lower", },
  { "-fintrin-case-any", },
  { "-fmatch-case-initcap", },
  { "-fmatch-case-upper", },
  { "-fmatch-case-lower", },
  { "-fmatch-case-any", },
  { "-fsource-case-upper", },
  { "-fsource-case-lower", },
  { "-fsource-case-preserve", },
  { "-fsymbol-case-initcap", },
  { "-fsymbol-case-upper", },
  { "-fsymbol-case-lower", },
  { "-fsymbol-case-any", },
  { "-fcase-strict-upper", },
  { "-fcase-strict-lower", },
  { "-fcase-initcap", },
  { "-fcase-upper", },
  { "-fcase-lower", },
  { "-fcase-preserve", },
  { "-fbadu77-intrinsics-delete", },
  { "-fbadu77-intrinsics-disable", },
  { "-fbadu77-intrinsics-enable", },
  { "-fbadu77-intrinsics-hide", },
  { "-fdcp-intrinsics-delete", },
  { "-fdcp-intrinsics-disable", },
  { "-fdcp-intrinsics-enable", },
  { "-fdcp-intrinsics-hide", },
  { "-ff2c-intrinsics-delete", },
  { "-ff2c-intrinsics-disable", },
  { "-ff2c-intrinsics-enable", },
  { "-ff2c-intrinsics-hide", },
  { "-ff90-intrinsics-delete", },
  { "-ff90-intrinsics-disable", },
  { "-ff90-intrinsics-enable", },
  { "-ff90-intrinsics-hide", },
  { "-fgnu-intrinsics-delete", },
  { "-fgnu-intrinsics-disable", },
  { "-fgnu-intrinsics-enable", },
  { "-fgnu-intrinsics-hide", },
  { "-fmil-intrinsics-delete", },
  { "-fmil-intrinsics-disable", },
  { "-fmil-intrinsics-enable", },
  { "-fmil-intrinsics-hide", },
  { "-funix-intrinsics-delete", },
  { "-funix-intrinsics-disable", },
  { "-funix-intrinsics-enable", },
  { "-funix-intrinsics-hide", },
  { "-fvxt-intrinsics-delete", },
  { "-fvxt-intrinsics-disable", },
  { "-fvxt-intrinsics-enable", },
  { "-fvxt-intrinsics-hide", },
  { "-fzeros", "Treat initial values of 0 as real initialisations" },
  { "-fno-zeros", "" },
  { "-fdebug-kludge", "Emit debugging information for COMMON and EQUIVALENCE" },
  { "-fno-debug-kludge", "" },
  { "-fonetrip", "Always perform DO loops at least once" },
  { "-fno-onetrip", "" },
  { "-fsilent", "" },
  { "-fno-silent", "Display names of program units as they are compiled" },
  { "-fglobals", "" },
  { "-fno-globals", "Disable diagnostics about inter-procedural problems" },
  { "-ftypeless-boz", "Make prefix-radix non-decimal constants be typeless" },
  { "-fno-typeless-boz", "" },
  { "-Wglobals", "" },
  { "-Wno-globals", "Inhibit warnings about globals used intrinsically" },
/*"-Wimplicit",*/
/*"-Wno-implicit",*/
  { "-Wsurprising", "Warn about suspicious constructs" },
  { "-Wno-surprising", "" },
/*"-Wall",*/
/* Prefix options.  */
  { "-I", "Add a directory for INCLUDE searching" },
  { "-ffixed-line-length-", "Set the maximum line length" },

#endif
