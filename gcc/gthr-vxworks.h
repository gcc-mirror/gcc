/* Threads compatibility routines for libgcc2 and libobjc for VxWorks.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1997, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Mike Stump <mrs@wrs.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#ifndef GCC_GTHR_VXWORKS_H
#define GCC_GTHR_VXWORKS_H

#ifdef _LIBOBJC

/* Thread local storage for a single thread */
static void *thread_local_storage = NULL;

/* Backend initialization functions */

/* Initialize the threads subsystem.  */
int
__gthread_objc_init_thread_system(void)
{
  /* No thread support available */
  return -1;
}

/* Close the threads subsystem.  */
int
__gthread_objc_close_thread_system(void)
{
  /* No thread support available */
  return -1;
}

/* Backend thread functions */

/* Create a new thread of execution.  */
objc_thread_t
__gthread_objc_thread_detach(void (*func)(void *arg), void *arg)
{
  /* No thread support available */
  return NULL;
}

/* Set the current thread's priority.  */
int
__gthread_objc_thread_set_priority(int priority)
{
  /* No thread support available */
  return -1;
}

/* Return the current thread's priority.  */
int
__gthread_objc_thread_get_priority(void)
{
  return OBJC_THREAD_INTERACTIVE_PRIORITY;
}

/* Yield our process time to another thread.  */
void
__gthread_objc_thread_yield(void)
{
  return;
}

/* Terminate the current thread.  */
int
__gthread_objc_thread_exit(void)
{
  /* No thread support available */
  /* Should we really exit the program */
  /* exit(&__objc_thread_exit_status); */
  return -1;
}

/* Returns an integer value which uniquely describes a thread.  */
objc_thread_t
__gthread_objc_thread_id(void)
{
  /* No thread support, use 1.  */
  return (objc_thread_t)1;
}

/* Sets the thread's local storage pointer.  */
int
__gthread_objc_thread_set_data(void *value)
{
  thread_local_storage = value;
  return 0;
}

/* Returns the thread's local storage pointer.  */
void *
__gthread_objc_thread_get_data(void)
{
  return thread_local_storage;
}

/* Backend mutex functions */

/* Allocate a mutex.  */
int
__gthread_objc_mutex_allocate(objc_mutex_t mutex)
{
  return 0;
}

/* Deallocate a mutex.  */
int
__gthread_objc_mutex_deallocate(objc_mutex_t mutex)
{
  return 0;
}

/* Grab a lock on a mutex.  */
int
__gthread_objc_mutex_lock(objc_mutex_t mutex)
{
  /* There can only be one thread, so we always get the lock */
  return 0;
}

/* Try to grab a lock on a mutex.  */
int
__gthread_objc_mutex_trylock(objc_mutex_t mutex)
{
  /* There can only be one thread, so we always get the lock */
  return 0;
}

/* Unlock the mutex */
int
__gthread_objc_mutex_unlock(objc_mutex_t mutex)
{
  return 0;
}

/* Backend condition mutex functions */

/* Allocate a condition.  */
int
__gthread_objc_condition_allocate(objc_condition_t condition)
{
  return 0;
}

/* Deallocate a condition.  */
int
__gthread_objc_condition_deallocate(objc_condition_t condition)
{
  return 0;
}

/* Wait on the condition */
int
__gthread_objc_condition_wait(objc_condition_t condition, objc_mutex_t mutex)
{
  return 0;
}

/* Wake up all threads waiting on this condition.  */
int
__gthread_objc_condition_broadcast(objc_condition_t condition)
{
  return 0;
}

/* Wake up one thread waiting on this condition.  */
int
__gthread_objc_condition_signal(objc_condition_t condition)
{
  return 0;
}

#else /* _LIBOBJC */

/* POSIX threads specific definitions.
   Easy, since the interface is just one-to-one mapping.  */

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
  /* Just reset the key value to zero.  */
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

#endif /* _LIBOBJC */

#endif /* ! GCC_GTHR_VXWORKS_H */
