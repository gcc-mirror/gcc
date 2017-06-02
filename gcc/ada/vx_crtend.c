/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                            V X _ C R T E N D                             *
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

/* crtend kind of file for ehframe registration/deregistration
   purposes on VxWorks.  */

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"

# if __INT_MAX__ == 2147483647
typedef int int32;
# elif __LONG_MAX__ == 2147483647
typedef long int32;
# elif __SHRT_MAX__ == 2147483647
typedef short int32;
# else
#  error "Missing a 4 byte integer"
# endif

static const int32 __FRAME_END__[]
     __attribute__ ((used, section(EH_FRAME_SECTION_NAME),
		     aligned(sizeof(int32))))
     = { 0 };
