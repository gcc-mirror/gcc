/* Definitions for specs for the GNU compiler for the Java(TM) language.
   Copyright (C) 1996, 1998, 1999, 2000, 2001, 2003, 2004, 2006, 2007
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.

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
    %{E:%{e-E is not valid for gcj}}\
    %{.java|fsaw-java-file:ecj1 %i %{W*} %{w} %{g*}			\
      %{fbootclasspath*}						\
      %{fenable-assertions*}						\
      %{fdisable-assertions*}						\
      %{fencoding*} %{ffilelist-file}					\
      %{foutput-class-dir*} %{g*}					\
      %{fsource*} %{!fsource*:-fsource=1.5}				\
      %{ftarget*} %{!femit-class-files|!ftarget*:-ftarget=1.5}		\
      %{!findirect-dispatch:-fzip-dependency %U.zip}			\
      %{!fsyntax-only:-fzip-target %U.jar}}\n				\
    %{.class|.zip|.jar|!fsyntax-only:jc1				\
      %{.java|fsaw-java-file:%U.jar -fsource-filename=%i %<ffilelist-file} \
      %{.class|.zip|.jar|ffilelist-file|fcompile-resource*:%i}		\
      %(jc1) %(cc1_options) %{I*} %{!findirect-dispatch:-faux-classpath %U.zip} \
      %{MD:-MD_} %{MMD:-MMD_} %{M} %{MM} %{MA} %{MT*} %{MF*}\
      %(invoke_as)}",
      0, 0, 0},

  /*
    FIXME: we don't use %|, even though we could, because we need the
    dependency zip to be ready early enough.  We could work around
    this by not having a dependency zip and instead teaching jc1 to
    read a special manifest file included in the sole zip, this
    manifest would say which files are to be compiled and which are
    not.
   */
