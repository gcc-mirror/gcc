/* lang-specs.h file for Fortran
   Copyright (C) 1995-1997 Free Software Foundation, Inc.
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

/* This is the contribution to the `default_compilers' array in gcc.c for
   g77.  */

#ifdef __STDC__	/* Else stringizing of OO below won't work, but in
		   K&R case we're not building the f77 language. */

#ifdef OBJECT_SUFFIX	/* Not defined compiling gcc.c prior to 2.7.0. */
#define OO "%O"
#else
#define OO ".o"
#endif

  {".F", "@f77-cpp-input"},
  {".fpp", "@f77-cpp-input"},
  {"@f77-cpp-input",
     /* For f77 we want -traditional to avoid errors with, for
	instance, mismatched '.  Also, we avoid unpleasant surprises
	with substitution of names not prefixed by `_' by using %P
	rather than %p (although this isn't consistent with SGI and
	Sun f77, at least) so you test `__unix' rather than `unix'.
	-D_LANGUAGE_FORTRAN is used by some compilers like SGI and
	might as well be in there. */
   "cpp -lang-c %{nostdinc*} %{C} %{v} %{A*} %{I*} %{P} %I\
	%{C:%{!E:%eGNU C does not support -C without using -E}}\
	%{M} %{MM} %{MD:-MD %b.d} %{MMD:-MMD %b.d} %{MG}\
	-undef -D__GNUC__=%v1 -D__GNUC_MINOR__=%v2\
	%{ansi:-trigraphs -$ -D__STRICT_ANSI__}\
	%{!undef:%P} -D_LANGUAGE_FORTRAN %{trigraphs} \
	%c %{O*:%{!O0:-D__OPTIMIZE__}} -traditional\
	%{g*} %{W*} %{w} %{pedantic*} %{H} %{d*} %C %{D*} %{U*} %{i*} %Z\
	%i %{!M:%{!MM:%{!E:%{!pipe:%g.i}}}}%{E:%W{o*}}%{M:%W{o*}}%{MM:%W{o*}} |\n",
   "%{!M:%{!MM:%{!E:f771 %{!pipe:%g.i} -fset-g77-defaults %(f771) \
		   %{!Q:-quiet} -dumpbase %b.F %{d*} %{m*} %{a}\
		   %{g*} %{O*} %{W*} %{w} %{pedantic*} \
		   %{v:-version -fversion} %{pg:-p} %{p} %{f*} %{I*}\
		   %{aux-info*}\
		   %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
		   %{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}} |\n\
	      %{!S:as %a %Y\
		      %{c:%W{o*}%{!o*:-o %w%b" OO "}}%{!c:-o %d%w%u" OO "}\
		      %{!pipe:%g.s} %A\n }}}}"},
  {".r", "@ratfor"},
  {"@ratfor",
   "ratfor %{C} %{v}\
           %{C:%{!E:%eGNU C does not support -C without using -E}}\
           %{!E:%{!pipe:-o %g.f}}%{E:%W{o*}} %i |\n",
   "%{!E:f771 %{!pipe:%g.f} -fset-g77-defaults %(f771) \
	   %{!Q:-quiet} -dumpbase %b.r %{d*} %{m*} %{a}\
	   %{g*} %{O*} %{W*} %{w} %{pedantic*} \
	   %{v:-version -fversion} %{pg:-p} %{p} %{f*} %{I*}\
	   %{aux-info*}\
	   %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
	   %{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}} |\n\
	   %{!S:as %a %Y\
	   %{c:%W{o*}%{!o*:-o %w%b" OO "}}%{!c:-o %d%w%u" OO "}\
           %{!pipe:%g.s} %A\n }}"},
  {".f", "@f77"},
  {".for", "@f77"},
  {"@f77",
   "%{!M:%{!MM:%{!E:f771 %i -fset-g77-defaults %(f771) \
		   %{!Q:-quiet} -dumpbase %b.f %{d*} %{m*} %{a}\
		   %{g*} %{O*} %{W*} %{w} %{pedantic*}\
		   %{v:-version -fversion} %{pg:-p} %{p} %{f*} %{I*}\
		   %{aux-info*}\
		   %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
		   %{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}} |\n\
	      %{!S:as %a %Y\
		      %{c:%W{o*}%{!o*:-o %w%b" OO "}}%{!c:-o %d%w%u" OO "}\
		      %{!pipe:%g.s} %A\n }}}}"},

#undef OO

#endif
