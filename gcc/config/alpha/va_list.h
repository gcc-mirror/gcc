/* A replacement for Digital Unix's <va_list.h>.

Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
typedef __builtin_va_list __gnuc_va_list;
#endif

#if !defined(_VA_LIST) && !defined(_HIDDEN_VA_LIST)
#define _VA_LIST
typedef __gnuc_va_list va_list;

#elif defined(_HIDDEN_VA_LIST) && !defined(_HIDDEN_VA_LIST_DONE)
#define _HIDDEN_VA_LIST_DONE
typedef __gnuc_va_list __va_list;

#elif defined(_HIDDEN_VA_LIST) && defined(_VA_LIST)
#undef _HIDDEN_VA_LIST

#endif
