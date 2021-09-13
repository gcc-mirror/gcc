/* Enable IBM PowerPC 476 support.
   Copyright (C) 2011-2021 Free Software Foundation, Inc.
   Contributed by Peter Bergner (bergner@vnet.ibm.com)
   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#undef TARGET_LINK_STACK
#define TARGET_LINK_STACK (rs6000_link_stack)

#undef SET_TARGET_LINK_STACK
#define SET_TARGET_LINK_STACK(X) do { TARGET_LINK_STACK = (X); } while (0)

#undef TARGET_ASM_CODE_END
#define TARGET_ASM_CODE_END rs6000_code_end
