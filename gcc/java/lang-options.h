/* Switch definitions for the GNU compiler for the Java(TM) language.
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

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

/* This is the contribution to the `documented_lang_options' array in
   toplev.c for java.  */

DEFINE_LANG_NAME ("Java")

  { "-fbounds-check", "" },
  { "-fno-bounds-check", "Disable automatic array bounds checking" },
  { "-fassume-compiled", "Make is_compiled_class return 1"},
  { "-fno-assume-compiled", "" },
  { "-femit-class-file", "" },
  { "-femit-class-files", "Dump class files to <name>.class" },
  { "-ffilelist-file", "input file is list of file names to compile" },
  { "-fuse-boehm-gc", "Generate code for Boehm GC" },
  { "-fhash-synchronization", "Don't put synchronization structure in each object" },
  { "-fjni", "Assume native functions are implemented using JNI" },
  { "--classpath", "Set class path and suppress system path" },
  { "--CLASSPATH", "Set class path" },
  { "--main", "Choose class whose main method should be used" },
  { "--encoding", "Choose input encoding (default is UTF-8)" },
  { "-I", "Add directory to class path" },
  { "-foutput-class-dir", "Directory where class files should be written" },
  { "-fuse-divide-subroutine", "" },
  { "-fno-use-divide-subroutine", "Use built-in instructions for division" },
  { "-fcheck-references", "Generate null pointer checks inline" },
  { "-Wredundant-modifiers", 
    "Warn if modifiers are specified when not necessary"},
  { "-Wextraneous-semicolon", "Warn if deprecated empty statements are found"},
  { "-Wout-of-date", "Warn if .class files are out of date" },
  { "-fforce-classes-archive-check", 
    "Always check for non gcj generated classes archives" },
