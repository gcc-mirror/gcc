/* Definitions for a picoJava Linux-based GNU system.
   Copyright (C) 2000, 2002 Free Software Foundation, Inc.

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

/* contributed by Steve Chamberlain, of Transmeta. sac@pobox.com. */

#define TARGET_LITTLE_ENDIAN_DEFAULT 1

#undef CPP_PREDEFINES
#undef STARTFILE_SPEC
#undef ENDFILE_SPEC

#define CPP_PREDEFINES "-D__ELF__ -Dunix -D__pj__ -D__gnu_linux__ -Dlinux -Asystem=posix"
#define STARTFILE_SPEC "crt1.o%s crti.o%s crtbegin.o%s"
#define ENDFILE_SPEC   "crtend.o%s crtn.o%s"

#undef WCHAR_TYPE_SIZE
#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"
#define WCHAR_TYPE_SIZE BITS_PER_WORD
