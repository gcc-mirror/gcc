/* Declarations for bare-metal C-SKY targets.
   Copyright (C) 2018 Free Software Foundation, Inc.
   Contributed by C-SKY Microsystems and Mentor Graphics.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


/******************************************************************
 *		 Run-time Target Specification			  *
 ******************************************************************/

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "crt0.o%s crti.o%s crtbegin.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

#undef CC1_SPEC
#define CC1_SPEC		 \
  "%{EB:-EB}			 \
   %{EL:-EL}			 \
   %{fpic|fPIC:-DPIC}		 \
   %{march=ck803s:-march=ck803}	 \
  "

#undef ASM_SPEC
#define ASM_SPEC		\
  "%{mbig-endian:-mbig-endian}	\
  %{EB:-EB}			\
  %{EL:-EL}			\
  %{fpic|fPIC:-pic}		\
  %{mcpu=*:-mcpu=%*}		\
  %{march=*:-march=%*}		\
  %{mhard-float:-mhard-float}	\
  %{melrw:-melrw}		\
  %{mno-elrw:-mno-elrw}		\
  %{mistack:-mistack}		\
  %{mno-istack:-mno-istack}	\
  %{mmp:-mmp}			\
  %{mcp:-mcp}			\
  %{mcache:-mcache}		\
  %{msecurity|mmac:-msecurity}	\
  %{mtrust:-mtrust}		\
  %{mdsp:-mdsp}			\
  %{medsp:-medsp}		\
  %{mvdsp:-mvdsp}		\
  "

#undef	LINK_SPEC
#define LINK_SPEC     \
"%{mbig-endian:-EB}   \
 %{EB:-EB}	      \
 %{EL:-EL} -X"

#undef	LIB_SPEC
#define LIB_SPEC \
  "%{pthread:-lpthread} -lc %{mccrt:-lcc-rt}"
/* FIXME add this to LIB_SPEC when need */
/*   %{!shared:%{profile:-lc_p}%{!profile:-lc}}" */


#define CPLUSPLUS_CPP_SPEC "-D_GNU_SOURCE %(cpp)"

/* Disable features only for Linux toolchains.	*/
#undef TARGET_POSIX_IO
#define TARGET_CSKY_LINUX 0
