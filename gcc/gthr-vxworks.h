/* Threads compatibility routines for libgcc2 for VxWorks.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1997 Free Software Foundation, Inc.
   Contributed by Mike Stump <mrs@wrs.com>.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#ifndef __gthr_vxworks_h
#define __gthr_vxworks_h

/* POSIX threads specific definitions.
   Easy, since the interface is just one-to-one mapping. */

#define __GTHREADS 1

#include <vxWorks.h>
#include <semLib.h>
/* typedef void *SEM_ID; */

typedef int __gthread_key_t;
typedef char __gthread_once_t;
typedef SEM_ID __gthread_mutex_t;

#define __GTHREAD_MUTEX_INIT 0
#define __GTHREAD_ONCE_INIT 0

#ifndef REG_SAVED_REG
static inline int
__gthread_once (__gthread_once_t *once, void (*func) (void))
{
  (*func)();
  return 0;
}

extern __gthread_key_t eh_context_key;

/* This is not the right way to do it, but the semantic of pthreads
   don't map well enough onto VxWorks.  */

static void
__ehdtor (void *pTcb)
{
  int tid = (int) pTcb;
  void *p = (void*)taskVarGet(tid, &eh_context_key);
  if (p != (void*)-1)
    {
      if (p)
	free (p);
      taskVarSet(tid, &eh_context_key, 0);
    }
}

/* This only works for the code in libgcc2.c.  */

static inline int
__gthread_key_create (__gthread_key_t *key, void (*dtor) (void *))
{
  *key = 0;

  /* Do this first so that the task variables are visible during the
     running of the delete hook.  */

  taskVarInit();

  /* We don't have a way to track dtor here, so instead, we
     register a generic routine that can cleanup any task.  */

  taskDeleteHookAdd (__ehdtor);

  return 0;
}

#define __gthread_setspecific(key, ptr) \
  (key = (int) ptr, 0)

static inline int
__gthread_key_dtor (__gthread_key_t key, void *ptr)
{
  /* Just reset the key value to zero. */
  if (ptr)
    return __gthread_setspecific (key, 0);
  else
    return 0;
}

#define __gthread_key_delete(key) \
  taskVarDelete (taskIdSelf (), &key)

#define __gthread_getspecific(key)			\
     ((key == 0)					\
      ? ((taskVarAdd (taskIdSelf (), &key) != OK)	\
	 ? (__terminate (), (void*)0)			\
	 : (void*)0)					\
      : (void*)key)
#endif

static inline int
__gthread_mutex_lock (__gthread_mutex_t *mutex)
{
  if (*mutex == 0)
    *mutex = semMCreate (SEM_Q_PRIORITY | SEM_INVERSION_SAFE | SEM_DELETE_SAFE);
  return semTake (*mutex, WAIT_FOREVER);
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t *mutex)
{
  if (*mutex == 0)
    *mutex = semMCreate (SEM_Q_PRIORITY | SEM_INVERSION_SAFE | SEM_DELETE_SAFE);
  return semTake (*mutex, NO_WAIT);
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t *mutex)
{
  /* We could return the */
  return semGive (*mutex);
}

#endif /* not __gthr_vxworks_h */
