/* lang-specs.h file for Fortran
   Copyright (C) 1995-1997, 1999 Free Software Foundation, Inc.
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

/* This is the contribution to the `default_compilers' array in gcc.c for
   g77.  */

  {".F", {"@f77-cpp-input"}},
  {".fpp", {"@f77-cpp-input"}},
  {".FPP", {"@f77-cpp-input"}},
  {"@f77-cpp-input",
     /* For f77 we want -traditional to avoid errors with, for
	instance, mismatched '.  Also, we avoid unpleasant surprises
	with substitution of names not prefixed by `_' by using %P
	rather than %p (although this isn't consistent with SGI and
	Sun f77, at least) so you test `__unix' rather than `unix'.
	-D_LANGUAGE_FORTRAN is used by some compilers like SGI and
	might as well be in there. */
   {"cpp -lang-fortran %{nostdinc*} %{C} %{v} %{A*} %{I*} %{P} %{$} %I\
	%{C:%{!E:%eGNU C does not support -C without using -E}}\
	%{M} %{MM} %{MD:-MD %b.d} %{MMD:-MMD %b.d} %{MG}\
	%{!no-gcc:-D__GNUC__=%v1 -D__GNUC_MINOR__=%v2}\
	%{ansi:-trigraphs -$ -D__STRICT_ANSI__}\
	%{!undef:%P} -D_LANGUAGE_FORTRAN %{trigraphs} \
	%c %{Os:-D__OPTIMIZE_SIZE__} %{O*:%{!O0:-D__OPTIMIZE__}} -traditional\
	%{ffast-math:-D__FAST_MATH__}\
	%{g*} %{W*} %{w} %{pedantic*} %{H} %{d*} %C %{D*} %{U*} %{i*} %Z\
	%i %{!M:%{!MM:%{!E:%{!pipe:%g.i}}}}%{E:%W{o*}}%{M:%W{o*}}%{MM:%W{o*}} |\n",
    "%{!M:%{!MM:%{!E:f771 %{!pipe:%g.i} %1 %(f771) \
		   %{!Q:-quiet} -dumpbase %b.F %{d*} %{m*} %{a*}\
		   %{g*} %{O*} %{W*} %{w} %{pedantic*} \
		   %{v:-version -fversion} %{pg:-p} %{p} %{f*} %{I*}\
		   %{aux-info*} %{Qn:-fno-ident}\
		   %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
		   %{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}} |\n\
	      %{!S:as %a %Y\
		      %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c:-o %d%w%u%O}\
		      %{!pipe:%g.s} %A\n }}}}"}},
  {".r", {"@ratfor"}},
  {"@ratfor",
   {"ratfor %{C} %{v}\
           %{C:%{!E:%eGNU C does not support -C without using -E}}\
           %{!E:%{!pipe:-o %g.f}}%{E:%W{o*}} %i |\n",
    "%{!E:f771 %{!pipe:%g.f} %1 %(f771) \
	   %{!Q:-quiet} -dumpbase %b.r %{d*} %{m*} %{a*}\
	   %{g*} %{O*} %{W*} %{w} %{pedantic*} \
	   %{v:-version -fversion} %{pg:-p} %{p} %{f*} %{I*}\
	   %{aux-info*} %{Qn:-fno-ident}\
	   %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
	   %{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}} |\n\
	   %{!S:as %a %Y\
	   %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c:-o %d%w%u%O}\
           %{!pipe:%g.s} %A\n }}"}},
  {".f", {"@f77"}},
  {".for", {"@f77"}},
  {".FOR", {"@f77"}},
  {"@f77",
   {"%{!M:%{!MM:%{!E:f771 %i %1 %(f771) \
		   %{!Q:-quiet} -dumpbase %b.f %{d*} %{m*} %{a*}\
		   %{g*} %{O*} %{W*} %{w} %{pedantic*}\
		   %{v:-version -fversion} %{pg:-p} %{p} %{f*} %{I*}\
		   %{aux-info*} %{Qn:-fno-ident}\
		   %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
		   %{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}} |\n\
	      %{!S:as %a %Y\
		      %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c:-o %d%w%u%O}\
		      %{!pipe:%g.s} %A\n }}}}"}},
  {"@f77-version",
   {"cpp -lang-fortran %{nostdinc*} %{C} %{v} %{A*} %{I*} %{P} %{$} %I \
      %{C:%{!E:%eGNU C does not support -C without using -E}} \
      %{M} %{MM} %{MD:-MD %b.d} %{MMD:-MMD %b.d} %{MG} \
      %{!no-gcc:-D__GNUC__=%v1 -D__GNUC_MINOR__=%v2} \
      %{ansi:-trigraphs -$ -D__STRICT_ANSI__} \
      %{!undef:%P} -D_LANGUAGE_FORTRAN %{trigraphs} \
      %c %{Os:-D__OPTIMIZE_SIZE__} %{O*:%{!O0:-D__OPTIMIZE__}} -traditional \
      %{ffast-math:-D__FAST_MATH__}\
      %{g*} %{W*} %{w} %{pedantic*} %{H} %{d*} %C %{D*} %{U*} %{i*} %Z \
      /dev/null /dev/null \n\
     f771 -fnull-version %1 %(f771) \
      %{!Q:-quiet} -dumpbase g77-version.f %{d*} %{m*} %{a*} \
      %{g*} %{O*} %{W*} %{w} %{pedantic*} \
      -version -fversion %{f*} %{I*} -o %g.s /dev/null \n\
     as %a %Y -o %g%O %g.s %A \n\
     ld %l %X -o %g %g%O %{A} %{d} %{e*} %{m} %{N} %{n} \
      %{r} %{s} %{t} %{u*} %{x} %{z} %{Z} \
      %{!A:%{!nostdlib:%{!nostartfiles:%S}}} \
      %{static:} %{L*} %D -lg2c -lm \
      %{!nostdlib:%{!nodefaultlibs:%G %L %G}} \
      %{!A:%{!nostdlib:%{!nostartfiles:%E}}} \
      %{T*} \n\
     %g \n"}},
