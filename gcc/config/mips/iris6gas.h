/* Definitions for MIPS running IRIX 6 using GNU AS
   Copyright (C) 2003, 2004
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Definitions of target machine for GNU compiler.  IRIX 6 with GNU as.  */

/* Override iris6.h version to always use -init/-fini.

   FIXME: integrate those use separate spec/define for this?  */
#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{mips4} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} %{w} \
%{!shared: %{!non_shared: %{!call_shared:%{!r: -call_shared -no_unresolved}}}} \
%{rpath} -init __do_global_ctors -fini __do_global_dtors \
%{shared:-hidden_symbol __do_global_ctors,__do_global_ctors_1,__do_global_dtors} \
-_SYSTYPE_SVR4 -woff 131 \
%{mabi=32: -32}%{mabi=n32: -n32}%{mabi=64: -64}%{!mabi*: -n32}"

/* Disable SHF_MERGE support.  Even if gas supports it, the IRIX 6 O32 ld
   does not without a special elspec(5) file.

   FIXME: Only do this if not using GNU ld.  */
#if HAVE_GAS_SHF_MERGE
#undef HAVE_GAS_SHF_MERGE
#define HAVE_GAS_SHF_MERGE (mips_abi != ABI_32)
#endif /* HAVE_GAS_SHF_MERGE */

/* There's no need to perform collecting with GNU as.  */
#undef COLLECT_PARSE_FLAG
