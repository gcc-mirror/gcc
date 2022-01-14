/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                         V X _ S T A C K _ I N F O                        *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *              Copyright (C) 2007-2009  Free Software Foundation, Inc.     *
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

/* VxWorks-specific file to have access to task-specific data and be able
   to extract the stack boundaries for stack checking. */

#include <taskLib.h>

typedef struct
{
  int  size;
  char *base;
  char *end;
} stack_info;

/* __gnat_get_stack_info is used by s-stchop.adb only for VxWorks. This
   procedure fills the stack information associated to the currently
   executing task. */
extern void __gnat_get_stack_info (stack_info *vxworks_stack_info);

void
__gnat_get_stack_info (stack_info *vxworks_stack_info)
{
  TASK_DESC descriptor;

  /* Ask the VxWorks kernel about stack values */
  taskInfoGet (taskIdSelf (), &descriptor);

  /* Fill the stack data with the information provided by the kernel */
  vxworks_stack_info->size = descriptor.td_stackSize;
  vxworks_stack_info->base = descriptor.td_pStackBase;
  vxworks_stack_info->end  = descriptor.td_pStackEnd;
}
