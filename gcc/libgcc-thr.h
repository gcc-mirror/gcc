/* Threads compatibily routines for libgcc2.  */
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

#ifndef __libgcc_thr_h
#define __libgcc_thr_h

/* If this file is compiled with threads support, it must
       #define __GTHREADS 1
   to indicate that threads support is present.
   
   The threads interface must define the following types:
     __gthread_key_t
     __gthread_once_t
     __gthread_mutex_t

   The threads interface must define the following macros:

     __GTHREAD_ONCE_INIT
     		to initialize __gthread_once_t
     __GTHREAD_MUTEX_INIT
     		to initialize __gthread_mutex_t to get a fast
		non-recursive mutex.

   The threads interface must define the following static functions:

     int __gthread_once (__gthread_once_t *once, void (*func) ())

     int __gthread_key_create (__gthread_key_t *keyp, void (*dtor) (void *))
     int __gthread_key_delete (__gthread_key_t key)

     void *__gthread_getspecific (__gthread_key_t key)
     int __gthread_setspecific (__gthread_key_t key, const void *ptr)

     int __gthread_mutex_lock (__gthread_mutex_t *mutex);
     int __gthread_mutex_trylock (__gthread_mutex_t *mutex);
     int __gthread_mutex_unlock (__gthread_mutex_t *mutex);

   All functions returning int should return 0 on success, -1 on error.

   Currently supported threads packages are
     POSIX threads with -D_PTHREADS
     DCE threads with -D_DCE_THREADS
     Solaris/UI threads with -D_SOLARIS_THREADS
*/

#if _PTHREADS
/* POSIX threads specific definitions.
   Easy, since the interface is just one-to-one mapping. */

#define __GTHREADS 1

#include <pthread.h>

typedef pthread_key_t __gthread_key_t;
typedef pthread_once_t __gthread_once_t;
typedef pthread_mutex_t __gthread_mutex_t;

#define __GTHREAD_MUTEX_INIT PTHREAD_MUTEX_INITIALIZER
#define __GTHREAD_ONCE_INIT PTHREAD_ONCE_INIT

static inline int
__gthread_once (__gthread_once_t *once, void (*func) ())
{
  return pthread_once (once, func);
}

static inline int
__gthread_key_create (__gthread_key_t *key, void (*dtor) (void *))
{
  return pthread_key_create (key, dtor);
}

static inline int
__gthread_key_delete (__gthread_key_t key)
{
  return pthread_key_delete (key);
}

static inline void *
__gthread_getspecific (__gthread_key_t key)
{
  return pthread_getspecific (key);
}

static inline int
__gthread_setspecific (__gthread_key_t key, const void *ptr)
{
  return pthread_setspecific (key, ptr);
}

static inline int
__gthread_mutex_lock (__gthread_mutex_t *mutex)
{
  return pthread_mutex_lock (mutex);
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t *mutex)
{
  return pthread_mutex_trylock (mutex);
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t *mutex)
{
  return pthread_mutex_unlock (mutex);
}

#elif _DCE_THREADS
/* DCE threads interface.
   DCE threads are based on POSIX threads draft 4, and many things
   have changed since then. */

#define __GTHREADS 1

#include <pthread.h>

typedef pthread_key_t __gthread_key_t;
typedef pthread_once_t __gthread_once_t;
typedef pthread_mutex_t __gthread_mutex_t;

#define __GTHREAD_ONCE_INIT pthread_once_init
/* Howto define __GTHREAD_MUTEX_INIT? */

static inline int
__gthread_once (__gthread_once_t *once, void (*func) ())
{
  return pthread_once (once, func);
}

static inline int
__gthread_key_create (__gthread_key_t *key, void (*dtor) (void *))
{
  return pthread_keycreate (key, dtor);
}

static inline int
__gthread_key_delete (__gthread_key_t key)
{
  return pthread_key_delete (key);
}

static inline void *
__gthread_getspecific (__gthread_key_t key)
{
  void *ptr;
  if (pthread_getspecific (key, &ptr) == 0)
    return ptr;
  else
    return 0;
}

static inline int
__gthread_setspecific (__gthread_key_t key, const void *ptr)
{
  return pthread_setspecific (key, (void *) ptr);
}

static inline int
__gthread_mutex_lock (__gthread_mutex_t *mutex)
{
  return pthread_mutex_lock (mutex);
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t *mutex)
{
  return pthread_mutex_trylock (mutex);
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t *mutex)
{
  return pthread_mutex_unlock (mutex);
}

#elif _SOLARIS_THREADS
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

static inline int
__gthread_once (__gthread_once_t *once, void (*func) ())
{
  if (once == 0 || func == 0)
    {
      errno = EINVAL;
      return -1;
    }

  if (once->once == 0)
    {
      if (mutex_lock (&once->mutex) != 0)
	return -1;
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
  return thr_keycreate (key, dtor);
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
  return mutex_lock (mutex);
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t *mutex)
{
  return mutex_trylock (mutex);
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t *mutex)
{
  return mutex_unlock (mutex);
}

#else /* no threads */

/* Just provide compatibility for mutex handling. */

typedef int __gthread_mutex_t;

#define __GTHREAD_MUTEX_INIT 0

static inline int
__gthread_mutex_lock (__gthread_mutex_t *mutex)
{
  return 0;
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t *mutex)
{
  return 0;
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t *mutex)
{
  return 0;
}

#endif /* no threads */

#endif /* not __libgcc_thr_h */
