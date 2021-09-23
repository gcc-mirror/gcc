/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                           I N I T I A L I Z E                            *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2021, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/*  This unit provides the default implementation of __gnat_initialize, which
    is called before the elaboration of the partition.  It is provided in a
    separate file so that users can replace it easily.  But the implementation
    should be empty on most targets.  */

#ifdef IN_RTS
#include "runtime.h"
#else
#include "config.h"
#include "system.h"
#endif

#include "raise.h"

#ifdef __cplusplus
extern "C" {
#endif

/******************************************/
/* __gnat_initialize (NT-mingw32 Version) */
/******************************************/

#if defined (__MINGW32__)

void
__gnat_initialize (void *eh)
{
   /* Install the Structured Exception handler.  */
   if (eh)
     __gnat_install_SEH_handler (eh);
}

#else

/***************************************/
/* __gnat_initialize (Default Version) */
/***************************************/

void
__gnat_initialize (void *eh ATTRIBUTE_UNUSED)
{
}

#endif

#ifdef __cplusplus
}
#endif
