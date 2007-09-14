/* MIPS SDE threads compatibility routines for libgcc2 and libobjc.  */
/* Compile this one with gcc.  */
/* Copyright (C) 2006, 2007 Free Software Foundation, Inc.
   Contributed by Nigel Stephens <nigel@mips.com>

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

#ifndef GCC_GTHR_MIPSSDE_H
#define GCC_GTHR_MIPSSDE_H

/* MIPS SDE threading API specific definitions.
   Easy, since the interface is pretty much one-to-one.  */

#define __GTHREADS 1

#include <sdethread.h>
#include <unistd.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef __sdethread_key_t __gthread_key_t;
typedef __sdethread_once_t __gthread_once_t;
typedef __sdethread_mutex_t __gthread_mutex_t;

typedef struct {
  long depth;
  __sdethread_t owner;
  __sdethread_mutex_t actual;
} __gthread_recursive_mutex_t;

#define __GTHREAD_MUTEX_INIT __SDETHREAD_MUTEX_INITIALIZER("gthr")
#define __GTHREAD_ONCE_INIT __SDETHREAD_ONCE_INIT
static inline int
__gthread_recursive_mutex_init_function(__gthread_recursive_mutex_t *mutex);
#define __GTHREAD_RECURSIVE_MUTEX_INIT_FUNCTION __gthread_recursive_mutex_init_function

#if SUPPORTS_WEAK && GTHREAD_USE_WEAK
# define __gthrw(name) \
  static __typeof(name) __gthrw_ ## name __attribute__ ((__weakref__(#name)));
# define __gthrw_(name) __gthrw_ ## name
#else
# define __gthrw(name)
# define __gthrw_(name) name
#endif

__gthrw(__sdethread_once)
__gthrw(__sdethread_key_create)
__gthrw(__sdethread_key_delete)
__gthrw(__sdethread_getspecific)
__gthrw(__sdethread_setspecific)

__gthrw(__sdethread_self)

__gthrw(__sdethread_mutex_lock)
__gthrw(__sdethread_mutex_trylock)
__gthrw(__sdethread_mutex_unlock)

__gthrw(__sdethread_mutex_init)

__gthrw(__sdethread_threading)

#if SUPPORTS_WEAK && GTHREAD_USE_WEAK

static inline int
__gthread_active_p (void)
{
  return !!(void *)&__sdethread_threading;
}

#else /* not SUPPORTS_WEAK */

static inline int
__gthread_active_p (void)
{
  return 1;
}

#endif /* SUPPORTS_WEAK */

static inline int
__gthread_once (__gthread_once_t *once, void (*func) (void))
{
  if (__gthread_active_p ())
    return __gthrw_(__sdethread_once) (once, func);
  else
    return -1;
}

static inline int
__gthread_key_create (__gthread_key_t *key, void (*dtor) (void *))
{
  return __gthrw_(__sdethread_key_create) (key, dtor);
}

static inline int
__gthread_key_delete (__gthread_key_t key)
{
  return __gthrw_(__sdethread_key_delete) (key);
}

static inline void *
__gthread_getspecific (__gthread_key_t key)
{
  return __gthrw_(__sdethread_getspecific) (key);
}

static inline int
__gthread_setspecific (__gthread_key_t key, const void *ptr)
{
  return __gthrw_(__sdethread_setspecific) (key, ptr);
}

static inline int
__gthread_mutex_lock (__gthread_mutex_t *mutex)
{
  if (__gthread_active_p ())
    return __gthrw_(__sdethread_mutex_lock) (mutex);
  else
    return 0;
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t *mutex)
{
  if (__gthread_active_p ())
    return __gthrw_(__sdethread_mutex_trylock) (mutex);
  else
    return 0;
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t *mutex)
{
  if (__gthread_active_p ())
    return __gthrw_(__sdethread_mutex_unlock) (mutex);
  else
    return 0;
}

static inline int
__gthread_recursive_mutex_init_function (__gthread_recursive_mutex_t *mutex)
{
  mutex->depth = 0;
  mutex->owner = __gthrw_(__sdethread_self) ();
  return __gthrw_(__sdethread_mutex_init) (&mutex->actual, NULL);
}

static inline int
__gthread_recursive_mutex_lock (__gthread_recursive_mutex_t *mutex)
{
  if (__gthread_active_p ())
    {
      __sdethread_t me = __gthrw_(__sdethread_self) ();

      if (mutex->owner != me)
	{
	  __gthrw_(__sdethread_mutex_lock) (&mutex->actual);
	  mutex->owner = me;
	}

      mutex->depth++;
    }
  return 0;
}

static inline int
__gthread_recursive_mutex_trylock (__gthread_recursive_mutex_t *mutex)
{
  if (__gthread_active_p ())
    {
      __sdethread_t me = __gthrw_(__sdethread_self) ();

      if (mutex->owner != me)
	{
	  if (__gthrw_(__sdethread_mutex_trylock) (&mutex->actual))
	    return 1;
	  mutex->owner = me;
	}

      mutex->depth++;
    }
  return 0;
}

static inline int
__gthread_recursive_mutex_unlock (__gthread_recursive_mutex_t *mutex)
{
  if (__gthread_active_p ())
    {
      if (--mutex->depth == 0)
	{
	   mutex->owner = (__sdethread_t) 0;
	   __gthrw_(__sdethread_mutex_unlock) (&mutex->actual);
	}
    }
  return 0;
}

#ifdef __cplusplus
}
#endif

#endif /* ! GCC_GTHR_MIPSSDE_H */
