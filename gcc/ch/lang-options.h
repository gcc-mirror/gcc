/* Definitions for switches for GNU CHILL.
   Copyright (C) 1995, 1998 Free Software Foundation, Inc.

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

/* This is the contribution to the `lang_options' array in gcc.c for
   CHILL.  */

/* CYGNUS LOCAL - the format of this file has been changed to
   allow cc1 to implement --help.  nickc/--help */

DEFINE_LANG_NAME ("Chill")
     
  { "-lang-chill", "" },
  { "-flocal-loop-counter", "" },
  { "-fno-local-loop-counter", "Do not make seperate scopes for every 'for' loop"},
  { "-fgrant-only", "Stop after successfully generating a grant file" },
  { "-fchill-grant-only", "" },
  { "-fold-strings", "Implement the 1984 Chill string semantics" },
  { "-fno-old-strings", "" },
  { "-fignore-case", "convert all idenitifers to lower case" },
  { "-fno-ignore-case", "" },
  { "-fpack", "Pack structures into available space"},
  { "-fno-pack", "" },
  { "-fspecial_UC", "Make special words be in uppercase" },
  { "-fspecial_LC", "" },
  { "-fruntime-checking", "" },
  { "-fno-runtime-checking", "Disable runtime checking of parameters" },
