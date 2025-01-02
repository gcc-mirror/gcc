/* Copyright (C) 2005-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "\
%{msim:%{!shared:crt0%O%s}} \
%{!msim:%{!mcpu=bf561*:%{!msdram:basiccrt%O%s} %{msdram:basiccrts%O%s};: \
		       %{!msdram:basiccrt561%O%s} %{msdram:basiccrt561s%O%s}} \
	%{mcpu=bf561*:%{mmulticore:%{!mcorea:%{!mcoreb:basiccrt561b%O%s}}}}} \
crti%O%s crtbegin%O%s crtlibid%O%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC	"crtend%O%s crtn%O%s"

#undef  LIB_SPEC
#define LIB_SPEC "--start-group -lc %{msim:-lsim}%{!msim:-lnosys} --end-group \
%{!T*:%{!msim:%{!msdram: \
	      %{mcpu=bf512*:-T bf512.ld%s}%{mcpu=bf514*:-T bf514.ld%s} \
	      %{mcpu=bf516*:-T bf516.ld%s}%{mcpu=bf518*:-T bf518.ld%s} \
	      %{mcpu=bf522*:-T bf522.ld%s}%{mcpu=bf523*:-T bf523.ld%s} \
	      %{mcpu=bf524*:-T bf524.ld%s}%{mcpu=bf525*:-T bf525.ld%s} \
	      %{mcpu=bf526*:-T bf526.ld%s}%{mcpu=bf527*:-T bf527.ld%s} \
	      %{mcpu=bf531*:-T bf531.ld%s}%{mcpu=bf532*:-T bf532.ld%s} \
	      %{mcpu=bf533*:-T bf533.ld%s}%{mcpu=bf534*:-T bf534.ld%s} \
	      %{mcpu=bf536*:-T bf536.ld%s}%{mcpu=bf537*:-T bf537.ld%s} \
	      %{mcpu=bf538*:-T bf538.ld%s}%{mcpu=bf539*:-T bf539.ld%s} \
	      %{mcpu=bf542*:-T bf542.ld%s}%{mcpu=bf544*:-T bf544.ld%s} \
	      %{mcpu=bf547*:-T bf547.ld%s}%{mcpu=bf548*:-T bf548.ld%s} \
	      %{mcpu=bf549*:-T bf549.ld%s} \
	      %{mcpu=bf561*:%{!mmulticore:-T bf561.ld%s} \
			    %{mmulticore:%{mcorea:-T bf561a.ld%s}} \
			    %{mmulticore:%{mcoreb:-T bf561b.ld%s}} \
			    %{mmulticore:%{!mcorea:%{!mcoreb:-T bf561m.ld%s}}}} \
	      %{mcpu=bf592*:-T bf592.ld%s} \
	      %{!mcpu=*:%eno processor type specified for linking} \
	      %{!mcpu=bf561*:-T bfin-common-sc.ld%s} \
	      %{mcpu=bf561*:%{!mmulticore:-T bfin-common-sc.ld%s} \
			   %{mmulticore:-T bfin-common-mc.ld%s}}}}}"

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"

#ifdef __BFIN_FDPIC__
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
asm (SECTION_OP); \
asm ("P3 = [SP + 20];\n\tcall " USER_LABEL_PREFIX #FUNC ";"); \
asm (TEXT_SECTION_ASM_OP);
#endif

#undef SUBTARGET_DRIVER_SELF_SPECS
#define SUBTARGET_DRIVER_SELF_SPECS \
     "%{mfdpic:-msim} %{mid-shared-library:-msim}"
