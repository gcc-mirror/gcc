/* Switch definitions for the GNU compiler for the Objective-C language.
   Copyright (C) 2000 Free Software Foundation, Inc.

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

/* This is the contribution to the `documented_lang_options' array in
   toplev.c for Objective-C.  */

DEFINE_LANG_NAME ("Objective C")
  
  { "-gen-decls", 
    N_("Dump decls to a .decl file") },
  { "-fgnu-runtime", 
    N_("Generate code for GNU runtime environment") },
  { "-fno-gnu-runtime", "" },
  { "-fnext-runtime", 
    N_("Generate code for NeXT runtime environment") },
  { "-fno-next-runtime", "" },
  { "-Wselector", 
    N_("Warn if a selector has multiple methods") },
  { "-Wno-selector", "" },
  { "-Wprotocol", "" },
  { "-Wno-protocol", 
    N_("Do not warn if inherited methods are unimplemented") },
  { "-print-objc-runtime-info",
    N_("Generate C header of platform specific features") },
  { "-fconstant-string-class",
    N_("Specify the name of the class for constant strings") },
