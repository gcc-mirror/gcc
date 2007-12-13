/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                         V X _ S T A C K _ I N F O                        *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *              Copyright (C) 2007, Free Software Foundation, Inc.          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
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
