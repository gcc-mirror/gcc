/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                          V X _ C R T B E G I N                           *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *              Copyright (C) 2016, Free Software Foundation, Inc.          *
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

/* Common body for the various flavors of vx_crtbegin C sources we need
   to perform eh frame registration/deregistration in various
   circumstances.  #includers should define CDTOR_VISIBILITY. */

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "unwind-dw2-fde.h"

/* Pick names that the VxWorks muncher will get if involved and leave the
   symbols with public visibility.  Hiding by default is pointless and even
   problematic in some configurations.  When the presence of these
   constructors is an issue, best is not to include the crt object files at
   all.  */

#define CTOR_NAME _STI__101___crtbe_register_frame
#define DTOR_NAME _STD__101___crtbe_deregister_frame

void CTOR_NAME (void) CTOR_ATTRIBUTE;
void DTOR_NAME (void) DTOR_ATTRIBUTE;

static const char __EH_FRAME_BEGIN__[]
__attribute__((section(EH_FRAME_SECTION_NAME), aligned(4)))
  = { };

void CTOR_NAME (void)
{
  static struct object object;
  __register_frame_info (__EH_FRAME_BEGIN__, &object);
}

void DTOR_NAME (void)
{
  __deregister_frame_info (__EH_FRAME_BEGIN__);
}

