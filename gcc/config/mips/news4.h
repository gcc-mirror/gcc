/* Definitions of target machine for GNU compiler.  Sony RISC NEWS (mips)
   Copyright (C) 1991, 1997 Free Software Foundation, Inc.

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

#define MIPS_NEWS

#define CPP_PREDEFINES "\
-Dr3000 -Dnews3700 -DLANGUAGE_C -DMIPSEB -DSYSTYPE_BSD \
-Dsony_news -Dsony -Dunix -Dmips -Dhost_mips \
-Asystem(unix) -Asystem(bsd) -Acpu(mips) -Amachine(mips)"

#define SYSTEM_INCLUDE_DIR "/usr/include2.0"

#define LIB_SPEC "%{p:-lprof1} %{pg:-lprof1} -lc"

#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"

#define MACHINE_TYPE "RISC NEWS-OS"

/* INITIALIZE_TRAMPOLINE calls this library function to flush
   program and data caches.  */
#define CACHE_FLUSH_FUNC "cacheflush"

