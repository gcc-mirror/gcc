/* Definitions for specs for the GNU compiler for the Java(TM) language.
   Copyright (C) 1996, 1998 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* This is the contribution to the `default_compilers' array in gcc.c for
   Java.  */

  {".java",   {"@java"} },
  {".class",  {"@java"} },
  {".zip",    {"@java"} },
  {".jar",    {"@java"} },
  {"@java",
   {"%{!E:jc1 %i %1 %{!Q:-quiet} %{d*} %{m*} %{a}\
		    %{g*} %{O*} %{W*} %{w} %{pedantic*} %{ansi}\
		    %{traditional} %{v:-version} %{pg:-p} %{p}\
		    %{f*} %{+e*} %{aux-info*}\
                    %{I*}\
		    %{MD} %{MMD} %{M} %{MM}\
		    %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
		    %{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}} |\n\
            %{!S:as %a %Y\
		    %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c:-o %d%w%u%O}\
		    %{!pipe:%g.s} %A\n }}"}},
