/* lang-specs.h file for Fortran
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

*/

/* This is the contribution to the `default_compilers' array in gcc.c for
   g77.  */

  {".F",   "@f77-cpp-input"},
  {".fpp", "@f77-cpp-input"},
  {".FPP", "@f77-cpp-input"},
  {"@f77-cpp-input",
   "tradcpp -lang-fortran %(cpp_options) %{!M:%{!MM:%{!E:%{!pipe:%g.f |\n\
    f771 %{!pipe:%g.f} %(cc1_options) %{!S:-o %{|!pipe:%g.s} |\n\
    as %(asm_options) %{!pipe:%g.s} %A }}}}}\n"},
  {".r", "@ratfor"},
  {"@ratfor",
   "%{C:%{!E:%eGNU C does not support -C without using -E}}\
    ratfor %{C} %{v} %i %{E:%W{o*}} %{!E: %{!pipe:-o %g.f} |\n\
    f771 %{!pipe:%g.f} %(cc1_options) %{!S:-o %{|!pipe:%g.s} |\n\
    as %(asm_options) %{!pipe:%g.s} %A }}\n"},
  {".f",   "@f77"},
  {".for", "@f77"},
  {".FOR", "@f77"},
  {"@f77",
   "%{!M:%{!MM:%{!E:f771 %i %(cc1_options) %{!S:-o %{|!pipe:%g.s} |\n\
     as %(asm_options) %{!pipe:%g.s} %A }}}}\n"},
  /* XXX This is perverse and should not be necessary.  */
  {"@f77-version",
   "tradcpp -lang-fortran %(cpp_options) /dev/null \n\
    f771 -fnull-version %1 \
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
     %g \n"},
