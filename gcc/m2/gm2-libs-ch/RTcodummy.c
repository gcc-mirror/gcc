/* RTcodummy.c provides dummy access to thread primitives.

Copyright (C) 2019-2023 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifdef __cplusplus
extern "C" {
#endif


void
RTco_wait (__attribute__ ((unused)) int sid)
{
}


void
RTco_signal (__attribute__ ((unused)) int sid)
{
}


int
RTco_init (void)
{
  return 0;
}


int
RTco_initSemaphore (__attribute__ ((unused)) int value)
{
  return 0;
}


/* signalThread signal the semaphore associated with thread tid.  */

void
RTco_signalThread (__attribute__ ((unused)) int tid)
{
}


/* waitThread wait on the semaphore associated with thread tid.  */

void
RTco_waitThread (__attribute__ ((unused)) int tid)
{
}


int
RTco_currentThread (void)
{
  return 0;
}


int
RTco_initThread (__attribute__ ((unused)) void (*proc)(void),
		 __attribute__ ((unused)) unsigned int stackSize,
		 __attribute__ ((unused)) unsigned int interruptLevel)
{
  return 0;
}


void
RTco_transfer (__attribute__ ((unused)) int *p1, __attribute__ ((unused)) int p2)
{
}


int
RTco_select (__attribute__ ((unused)) int p1,
	     __attribute__ ((unused)) void *p2,
	     __attribute__ ((unused)) void *p3,
	     __attribute__ ((unused)) void *p4,
	     __attribute__ ((unused)) void *p5)
{
  return 0;
}


unsigned int
RTco_currentInterruptLevel (void)
{
  return 0;
}


/* turninterrupts returns the old interrupt level and assigns the interrupt level
   to newLevel.  */

unsigned int
RTco_turnInterrupts (unsigned int newLevel)
{
  return 0;
}

void
_M2_RTco_init (void)
{
}

void
_M2_RTco_finish (void)
{
}


#ifdef __cplusplus
}
#endif
