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

/* The VxWorks 7 environment for C++ is based on a Dinkumware toolchain,
   where quite a few configuration parameters are exposed in a yvals.h header
   file which needs to be included before other standard headers.

   This header file provides a wrapper facility to achieve this in addition
   to the common requirement to include vxWorks.h before anything else.  */

#include <_vxworks-versions.h>

#if _VXWORKS_MAJOR_GE(7) && defined(__cplusplus)

#include <_yvals.h>

#pragma push_macro("__cplusplus")
#undef __cplusplus
#include_next __HEADER_TO_WRAP
#pragma pop_macro("__cplusplus")

#else

#include_next __HEADER_TO_WRAP

#endif
