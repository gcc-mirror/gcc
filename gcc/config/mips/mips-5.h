/* Definitions of target machine for GNU compiler.  MIPS RISC-OS 5.0
   default version.
   Copyright (C) 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define LINK_SPEC "\
%{G*} \
%{!mgas: \
	%{EB} %{!EB: -EB} \
	%{EL: %e-EL not supported} \
	%{mips1} %{mips2} %{mips3} %{bestGnum} \
	%{shared} %{non_shared} %{call_shared} %{no_archive} %{exact_version} \
	%{!shared: %{!non_shared: %{!call_shared: -non_shared}}}}"

#include "mips.h"
