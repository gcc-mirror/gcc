/* Definitions for specs for GNU CHILL.
   Copyright (C) 1995, 1998, 1999 Free Software Foundation, Inc..

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This is the contribution to the `default_compilers' array in gcc.c for
   CHILL.  */

  {".ch",  {"@chill"}},
  {".chi", {"@chill"}},
  {"@chill",
     {"cpp -lang-chill %{nostdinc*} %{C} %{v} %{A*} %{I*} %{P} %{$} %I\
	%{C:%{!E:%eGNU CHILL does not support -C without using -E}}\
        %{!no-gcc:-D__GNUCHILL__=%v1 -D__GNUC_MINOR__=%v2}\
        %c %{Os:-D__OPTIMIZE_SIZE__} %{O*:-D__OPTIMIZE__} %{traditional} %{ftraditional:-traditional}\
        %{traditional-cpp:-traditional} %{!undef:%{!ansi:%p} %P} %{trigraphs}\
	%{g*} %{W*} %{w} %{pedantic*} %{H} %{d*} %C %{D*} %{U*} %{i*} %Z\
        %i %{!E:%g.i}%{E:%W{o*}} \n",
   "%{!E:cc1chill %g.i %1 \
		   %{!Q:-quiet} -dumpbase %b.ch %{d*} %{m*} %{a}\
		   %{g*} %{O*} %{W*} %{w} %{pedantic*} %{itu} \
		   %{v:-version} %{pg:-p} %{p} %{f*} %{I*} \
		   %{aux-info*} %{Qn:-fno-ident} %X \
		   %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
		   %{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}} |\n\
              %{!S:as %a %Y \
		      %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c:-o %d%w%u%O}\
                      %{!pipe:%g.s} %A\n }}"}},
