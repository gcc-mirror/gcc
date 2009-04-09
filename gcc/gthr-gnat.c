/* Threads compatibility routines for libgcc2.  */
/* Compile this one with gcc.  */
/* Copyright (C) 2003, 2004, 2009 Free Software Foundation, Inc.

This file is part of GCC.

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

#include "gthr-gnat.h"

#ifndef HIDE_EXPORTS
#pragma GCC visibility push(default)
#endif

#ifdef __cplusplus
#define UNUSED(x)
#else
#define UNUSED(x) x __attribute__((unused))
#endif

void __gnat_default_lock (void);
void __gnat_default_unlock (void);

void
__gnat_default_lock (void)
{
  return;
}

void
__gnat_default_unlock (void)
{
  return;
}

static void (*__gnat_task_lock) (void) = *__gnat_default_lock;
static void (*__gnat_task_unlock) (void) = *__gnat_default_unlock;

 void
__gnat_install_locks (void (*lock) (void), void (*unlock) (void))
{
  __gnat_task_lock = lock;
  __gnat_task_unlock = unlock;
}

int
__gthread_active_p (void)
{
  return 0;
}

int
__gthread_mutex_lock (__gthread_mutex_t * UNUSED (mutex))
{
  __gnat_task_lock ();
  return 0;
}

int
__gthread_mutex_unlock (__gthread_mutex_t * UNUSED (mutex))
{
  __gnat_task_unlock ();
  return 0;
}

#ifndef HIDE_EXPORTS
#pragma GCC visibility pop
#endif
