/* Threads compatibility routines for libgcc2.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1997 Free Software Foundation, Inc.

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

#ifndef __gthr_solaris_h
#define __gthr_solaris_h

/* Solaris threads as found in Solaris 2.[456].
   Actually these are Unix International (UI) threads, but I don't
   know if anyone else implements these. */

#define __GTHREADS 1

#include <thread.h>
#include <errno.h>

typedef thread_key_t __gthread_key_t;
typedef struct
{
  mutex_t mutex;
  int once;
} __gthread_once_t;
typedef mutex_t __gthread_mutex_t;

#define __GTHREAD_ONCE_INIT { DEFAULTMUTEX, 0 }
#define __GTHREAD_MUTEX_INIT DEFAULTMUTEX

#if SUPPORTS_WEAK && GTHREAD_USE_WEAK

#pragma weak thr_keycreate
#pragma weak thr_getspecific
#pragma weak thr_setspecific
#pragma weak thr_create

#pragma weak mutex_lock
#pragma weak mutex_trylock
#pragma weak mutex_unlock

/* This will not actually work in Solaris 2.5, since libc contains
   dummy symbols of all thr_* routines. */

static void *__gthread_active_ptr = &thr_create;

static inline int
__gthread_active_p ()
{
  return __gthread_active_ptr != 0;
}

#else /* not SUPPORTS_WEAK */

static inline int
__gthread_active_p ()
{
  return 1;
}

#endif /* SUPPORTS_WEAK */

static inline int
__gthread_once (__gthread_once_t *once, void (*func) ())
{
  if (! __gthread_active_p ())
    return -1;

  if (once == 0 || func == 0)
    return EINVAL;

  if (once->once == 0)
    {
      int status = mutex_lock (&once->mutex);
      if (status != 0)
	return status;
      if (once->once == 0)
	{
	  (*func) ();
	  once->once ++;
	}
      mutex_unlock (&once->mutex);
    }
  return 0;
}

static inline int
__gthread_key_create (__gthread_key_t *key, void (*dtor) (void *))
{
  /* Solaris 2.5 contains thr_* routines no-op in libc, so test if we actually
     got a reasonable key value, and if not, fail. */
  *key = -1;
  if (thr_keycreate (key, dtor) != 0 || *key == -1)
    return -1;
  else
    return 0;
}

static inline int
__gthread_key_dtor (__gthread_key_t key, void *ptr)
{
  /* Nothing needed. */
  return 0;
}

static inline int
__gthread_key_delete (__gthread_key_t key)
{
  /* Not possible. */
  return -1;
}

static inline void *
__gthread_getspecific (__gthread_key_t key)
{
  void *ptr;
  if (thr_getspecific (key, &ptr) == 0)
    return ptr;
  else
    return 0;
}

static inline int
__gthread_setspecific (__gthread_key_t key, const void *ptr)
{
  return thr_setspecific (key, (void *) ptr);
}

static inline int
__gthread_mutex_lock (__gthread_mutex_t *mutex)
{
  if (__gthread_active_p ())
    return mutex_lock (mutex);
  else
    return 0;
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t *mutex)
{
  if (__gthread_active_p ())
    return mutex_trylock (mutex);
  else
    return 0;
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t *mutex)
{
  if (__gthread_active_p ())
    return mutex_unlock (mutex);
  else
    return 0;
}

#endif /* not __gthr_solaris_h */
