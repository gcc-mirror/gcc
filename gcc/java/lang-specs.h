/* Definitions for specs for the GNU compiler for the Java(TM) language.
   Copyright (C) 1996, 1998, 1999, 2000, 2001, 2003, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* This is the contribution to the `default_compilers' array in gcc.c for
   Java.  */

  {".java",   "@java" , 0, 0, 0},
  {".class",  "@java" , 0, 0, 0},
  {".zip",    "@java" , 0, 0, 0},
  {".jar",    "@java" , 0, 0, 0},
  {"@java",
   "%{fjni:%{femit-class-files:%e-fjni and -femit-class-files are incompatible}}\
    %{fjni:%{femit-class-file:%e-fjni and -femit-class-file are incompatible}}\
    %{femit-class-file:%{!fsyntax-only:%e-femit-class-file should used along with -fsyntax-only}}\
    %{femit-class-files:%{!fsyntax-only:%e-femit-class-file should used along with -fsyntax-only}}\
    %{!E:jc1 %i %(jc1) %(cc1_options) %{+e*} %{I*}\
             %{MD:-MD_} %{MMD:-MMD_} %{M} %{MM} %{MA} %{MT*} %{MF*}\
             %{!fsyntax-only:%(invoke_as)}}", 0, 0, 0},

