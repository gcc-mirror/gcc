/* This file is part of GCC.

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

#ifndef __GCC_SETJMP_H
#define __GCC_SETJMP_H

/* Getting proper namespaces for c++ requires that don't alter the perception
   that we are compiling for c++, unlike what _yvals-wrapper would do.
   setjmp.h redoes a few of a the yvals.h things in its own fashion, so the
   pre-inclusion of yvals.h on VxWorks 7 with __cplusplus defined isn't a
   problem in this particular case.  */

#include <_yvals.h>
#include_next  <setjmp.h>

#endif
