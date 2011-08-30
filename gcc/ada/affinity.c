/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             A F F I N I T Y                              *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *            Copyright (C) 2005-2011, Free Software Foundation, Inc.       *
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

/* VxWorks SMP CPU affinity */

#include "taskLib.h"
#include "cpuset.h"

extern int __gnat_set_affinity (int tid, unsigned cpu);
extern int __gnat_set_affinity_mask (int tid, unsigned mask);

int
 __gnat_set_affinity (int tid, unsigned cpu)
{
  cpuset_t cpuset;

  CPUSET_ZERO(cpuset);
  CPUSET_SET(cpuset, cpu);
  return taskCpuAffinitySet (tid, cpuset);
}

int
__gnat_set_affinity_mask (int tid, unsigned mask)
{
  cpuset_t cpuset;

  CPUSET_ZERO(cpuset);

  for (index = 0; index < sizeof (unsigned) * 8; index++)
    if (mask & (1 << index))
      CPUSET_SET(cpuset, index);

  return taskCpuAffinitySet (tid, cpuset);
}
