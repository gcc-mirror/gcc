/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             A F F I N I T Y                              *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *            Copyright (C) 2005-2021, Free Software Foundation, Inc.       *
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

/* VxWorks SMP CPU affinity */

#include "taskLib.h"
#include "cpuset.h"

extern int __gnat_set_affinity (TASK_ID tid, unsigned cpu);
extern int __gnat_set_affinity_mask (TASK_ID tid, unsigned mask);

int
 __gnat_set_affinity (TASK_ID tid, unsigned cpu)
{
  cpuset_t cpuset;

  CPUSET_ZERO(cpuset);
  CPUSET_SET(cpuset, cpu);
  return taskCpuAffinitySet (tid, cpuset);
}

int
__gnat_set_affinity_mask (TASK_ID tid, unsigned mask)
{
  unsigned index;
  cpuset_t cpuset;

  CPUSET_ZERO(cpuset);

  for (index = 0; index < sizeof (unsigned) * 8; index++)
    if (mask & (1 << index))
      CPUSET_SET(cpuset, index);

  return taskCpuAffinitySet (tid, cpuset);
}
