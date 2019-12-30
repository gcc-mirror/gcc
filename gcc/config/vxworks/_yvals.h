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

#ifndef __GCC_YVALS_H
#define __GCC_YVALS_H

#include <_vxworks-versions.h>

#if _VXWORKS_MAJOR_GE(7)

/* We need to deactivate the definitions tailored for the Dinkumware
   intrinsics, incompatible with a compilation by G++.  */

#include <yvals.h>

#undef _HAS_CPP17
#define _HAS_CPP17 0

#undef _HAS_CPP14
#define _HAS_CPP14 0

#undef _HAS_CPP11
#define _HAS_CPP11 0

#endif /* VxWorks MAJOR >= 7 */

#endif /* __GCC_YVALS_H */
