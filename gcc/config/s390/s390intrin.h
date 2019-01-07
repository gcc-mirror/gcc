/* S/390 System z specific intrinsics
   Copyright (C) 2013-2019 Free Software Foundation, Inc.
   Contributed by Andreas Krebbel (Andreas.Krebbel@de.ibm.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef  _S390INTRIN_H
#define _S390INTRIN_H

#ifndef __s390__
  #error s390intrin.h included on wrong platform/compiler
#endif

#ifdef __HTM__
#include <htmintrin.h>
#endif

#ifdef __VEC__
#include <vecintrin.h>
#endif

#endif /* _S390INTRIN_H*/
