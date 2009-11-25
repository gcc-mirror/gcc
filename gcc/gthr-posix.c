/* POSIX threads dummy routines for systems without weak definitions.  */
/* Compile this one with gcc.  */
/* Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009 Free Software Foundation, Inc.

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

#include "tconfig.h"
#include "tm.h"
# define __gthrw_pragma(pragma) _Pragma (#pragma)
/* Define so we provide weak definitions of functions used by libobjc only.  */
#define _LIBOBJC_WEAK
#include "gthr.h"

int
pthread_once (pthread_once_t *once ATTRIBUTE_UNUSED,
	      void (*func) (void) ATTRIBUTE_UNUSED)
{
  return -1;
}

int
pthread_key_create (pthread_key_t *key ATTRIBUTE_UNUSED,
		    void (*dtor) (void *) ATTRIBUTE_UNUSED)
{
  return -1;
}

int
pthread_key_delete (pthread_key_t key ATTRIBUTE_UNUSED)
{
  return 0;
}

void *
pthread_getspecific (pthread_key_t key ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_setspecific (pthread_key_t key ATTRIBUTE_UNUSED,
		     const void *ptr ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_create (pthread_t *thread ATTRIBUTE_UNUSED,
		const pthread_attr_t *attr ATTRIBUTE_UNUSED,
		void *(*start_routine) (void *) ATTRIBUTE_UNUSED,
		void *arg ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_join (pthread_t thread ATTRIBUTE_UNUSED,
	      void **value_ptr ATTRIBUTE_UNUSED)
{
  return 0;
}

void
pthread_exit (void *value_ptr ATTRIBUTE_UNUSED)
{
}

int
pthread_detach (pthread_t thread ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_cancel (pthread_t thread ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_mutex_lock (pthread_mutex_t *mutex ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_mutex_trylock (pthread_mutex_t *mutex ATTRIBUTE_UNUSED)
{
  return 0;
}

#ifdef _POSIX_TIMEOUTS
#if _POSIX_TIMEOUTS >= 0
int
pthread_mutex_timedlock (pthread_mutex_t *mutex ATTRIBUTE_UNUSED,
			 const struct timespec *abs_timeout ATTRIBUTE_UNUSED)
{
  return 0;
}
#endif
#endif /* _POSIX_TIMEOUTS */

int
pthread_mutex_unlock (pthread_mutex_t *mutex ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_mutexattr_init (pthread_mutexattr_t *attr ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_mutexattr_settype (pthread_mutexattr_t *attr ATTRIBUTE_UNUSED,
			   int type ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_mutexattr_destroy (pthread_mutexattr_t *attr ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_cond_broadcast (pthread_cond_t *cond ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_cond_destroy (pthread_cond_t *cond ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_cond_init (pthread_cond_t *cond ATTRIBUTE_UNUSED,
		   const pthread_condattr_t *attr ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_cond_signal (pthread_cond_t *cond ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_cond_wait (pthread_cond_t *cond ATTRIBUTE_UNUSED,
		   pthread_mutex_t *mutex ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_cond_timedwait (pthread_cond_t *cond ATTRIBUTE_UNUSED, 
			pthread_mutex_t *mutex ATTRIBUTE_UNUSED,
			const struct timespec *abstime ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_mutex_init (pthread_mutex_t *mutex ATTRIBUTE_UNUSED,
		    const pthread_mutexattr_t *attr ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_mutex_destroy (pthread_mutex_t *mutex ATTRIBUTE_UNUSED)
{
  return 0;
}

pthread_t
pthread_self (void)
{
  return (pthread_t) 0;
}

#ifdef _POSIX_PRIORITY_SCHEDULING
#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
int
sched_get_priority_max (int policy ATTRIBUTE_UNUSED)
{
  return 0;
}

int
sched_get_priority_min (int policy ATTRIBUTE_UNUSED)
{
  return 0;
}
#endif /* _POSIX_THREAD_PRIORITY_SCHEDULING */
#endif /* _POSIX_PRIORITY_SCHEDULING */

int
sched_yield (void)
{
  return 0;
}

int
pthread_attr_destroy (pthread_attr_t *attr ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_attr_init (pthread_attr_t *attr ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_attr_setdetachstate (pthread_attr_t *attr ATTRIBUTE_UNUSED,
			     int detachstate ATTRIBUTE_UNUSED)
{
  return 0;
}

#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
int
pthread_getschedparam (pthread_t thread ATTRIBUTE_UNUSED,
		       int *policy ATTRIBUTE_UNUSED,
		       struct sched_param *param ATTRIBUTE_UNUSED)
{
  return 0;
}

int
pthread_setschedparam (pthread_t thread ATTRIBUTE_UNUSED,
		       int policy ATTRIBUTE_UNUSED,
		       const struct sched_param *param ATTRIBUTE_UNUSED)
{
  return 0;
}
#endif /* _POSIX_THREAD_PRIORITY_SCHEDULING */

