/* Switch definitions for the GNU compiler for the Java(TM) language.
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* This is the contribution to the `lang_options' array in gcc.c for
   java.  */

DEFINE_LANG_NAME ("Java")

  { "-fbounds-check", "" },
  { "-fno-bounds-check", "Disable automatic array bounds checking" },
  { "-fassume-compiled", "Make is_compiled_class return 1"},
  { "-fno-assume-compiled", "" },
  { "-femit-class-file", "" },
  { "-femit-class-files", "Dump class files to <name>.class" },
  { "-fuse-boehm-gc", "Generate code for Boehm GC" },
#if ! USE_CPPLIB
  { "-MD", "Print dependencies to FILE.d" },
  { "-MMD", "Print dependencies to FILE.d" },
  { "-M", "Print dependencies to stdout" },
  { "-MM", "Print dependencies to stdout" },
#endif /* ! USE_CPPLIB */
  { "-fclasspath", "Set class path and suppress system path" },
  { "-fCLASSPATH", "Set class path" },
  { "-I", "Add directory to class path" },
  { "-foutput-class-dir", "Directory where class files should be written" },
  { "-fuse-divide-subroutine", "" },
  { "-fno-use-divide-subroutine", "Use built-in instructions for division" },
  { "-Wredundant-modifiers", 
    "Warn if modifiers are specified when not necessary"},
  { "-Wunsupported-jdk11", "Warn if `final' local variables are specified"}
