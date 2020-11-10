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

#ifndef _VXWORKS_VERSIONS_H
#define _VXWORKS_VERSIONS_H  1

/* All we need is access to the bare _WRS_VXWORKS_MAJOR/MINOR macros,
   exposed by version.h or already provided somehow (e.g. with a self
   spec for some reason).  When resorting to system headers, cheat a
   bit to make sure we don't drag additional header files, which can
   easily cause #include ordering nightmares.  */

#if !defined(_WRS_VXWORKS_MAJOR)
#pragma push_macro("_WRS_KERNEL")
#undef _WRS_KERNEL
#include <version.h>
#pragma pop_macro("_WRS_KERNEL")
#endif

/* A lot depends on the MAJOR so we really need to make sure we have
   that.  MINOR is less critical and many environments don't actually
   define it unless it is really meaningful (e.g. 6.4 through 6.9).  */

#if !defined(_WRS_VXWORKS_MAJOR)
#error "_WRS_VXWORKS_MAJOR undefined"
#endif

#if !defined(_WRS_VXWORKS_MINOR)
#define _WRS_VXWORKS_MINOR 0
#endif

#define _VXWORKS_MAJOR_GT(MAJOR) (_WRS_VXWORKS_MAJOR > (MAJOR))
#define _VXWORKS_MAJOR_GE(MAJOR) (_WRS_VXWORKS_MAJOR >= (MAJOR))
#define _VXWORKS_MAJOR_LT(MAJOR) (_WRS_VXWORKS_MAJOR < (MAJOR))
#define _VXWORKS_MAJOR_LE(MAJOR) (_WRS_VXWORKS_MAJOR <= (MAJOR))
#define _VXWORKS_MAJOR_EQ(MAJOR) (_WRS_VXWORKS_MAJOR == (MAJOR))

#define _VXWORKS_MINOR_GT(MINOR) (_WRS_VXWORKS_MINOR > (MINOR))
#define _VXWORKS_MINOR_GE(MINOR) (_WRS_VXWORKS_MINOR >= (MINOR))
#define _VXWORKS_MINOR_LT(MINOR) (_WRS_VXWORKS_MINOR < (MINOR))
#define _VXWORKS_MINOR_LE(MINOR) (_WRS_VXWORKS_MINOR <= (MINOR))
#define _VXWORKS_MINOR_EQ(MINOR) (_WRS_VXWORKS_MINOR == (MINOR))

#define _VXWORKS_PRE(MAJOR,MINOR) \
  (_VXWORKS_MAJOR_LT(MAJOR) \
   || (_VXWORKS_MAJOR_EQ(MAJOR) && _VXWORKS_MINOR_LT(MINOR)))

#endif
