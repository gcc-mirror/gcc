/* Threads compatibily routines for libgcc2.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1999 Free Software Foundation, Inc.
   Contributed by Mumit Khan <khan@xraylith.wisc.edu>.

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

#ifndef __gthr_win32_h
#define __gthr_win32_h

/* Windows32 threads specific definitions. The windows32 threading model
   does not map well into pthread-inspired gcc's threading model, and so 
   there are caveats one needs to be aware of.

   1. The destructor supplied to __gthread_key_create is ignored. This
      will certainly cause memory leaks due to unreclaimed eh contexts
      (sizeof (eh_context) is at least 24 bytes for x86 currently).

      This memory leak may be significant for long-running applications
      that make heavy use of C++ EH.

   2. The error codes returned are non-POSIX like, and cast into ints.
      This may cause incorrect error return due to truncation values on 
      hw where sizeof (DWORD) > sizeof (int).
  
   The basic framework should work well enough. */

#define __GTHREADS 1

#include <windows.h>
#include <errno.h>

typedef DWORD __gthread_key_t;

typedef struct {
  int done;
  long started;
} __gthread_once_t;

typedef HANDLE __gthread_mutex_t;

#define __GTHREAD_ONCE_INIT {FALSE, -1}
#define __GTHREAD_MUTEX_INIT_FUNCTION __gthread_mutex_init_function

static inline int
__gthread_active_p ()
{
  return 1;
}

static inline int
__gthread_once (__gthread_once_t *once, void (*func) ())
{
  if (! __gthread_active_p ())
    return -1;
  else if (once == NULL || func == NULL)
    return EINVAL;

  if (! once->done)
    {
      if (InterlockedIncrement (&(once->started)) == 0)
        {
	  (*func) ();
	  once->done = TRUE;
	}
    }
  else
    {
      /* Another thread is currently executing the code, so wait for it to
         finish; yield the CPU in the meantime.  */ 
      while (! once->done)
        Sleep (0);
    }
  
  return 0;
}

/* Windows32 thread local keys don't support destructors; to avoid leaks,
   we will have to figure something out in the future.  */
static inline int
__gthread_key_create (__gthread_key_t *key, 
                      void (*dtor) (void *) __attribute__((__unused__)))
{
  int status = 0;
  DWORD tls_index = TlsAlloc ();
  if (tls_index != 0xFFFFFFFF)
    *key = tls_index;
  else
    status = (int) GetLastError ();
  return status;
}

/* Currently, this routine is never called since win32 keys don't support
   destructors. Hopefully we'll find a way in the future.  */
static inline int
__gthread_key_dtor (__gthread_key_t key, void *ptr)
{
  int status = 0;

  /* Just reset the key value to zero. */
  if (ptr)
    status = (TlsSetValue (key, 0) != 0) ? 0 : (int) GetLastError ();
  return status;
}

/* Currently, this routine is never called since win32 keys don't support
   destructors. Hopefully we'll find a way in the future.  */
static inline int
__gthread_key_delete (__gthread_key_t key)
{
  return (TlsFree (key) != 0) ? 0 : (int) GetLastError ();
}

static inline void *
__gthread_getspecific (__gthread_key_t key)
{
  return TlsGetValue (key);
}

static inline int
__gthread_setspecific (__gthread_key_t key, const void *ptr)
{
  return (TlsSetValue (key, ptr) != 0) ? 0 : (int) GetLastError ();
}

static inline void
__gthread_mutex_init_function (__gthread_mutex_t *mutex)
{
  /* Create unnamed mutex with default security attr and no initial owner.  */ 
  *mutex = CreateMutex (NULL, 0, NULL);
}

static inline int
__gthread_mutex_lock (__gthread_mutex_t *mutex)
{
  int status = 0;

  if (__gthread_active_p ())
    {
      if (WaitForSingleObject (*mutex, INFINITE) == WAIT_OBJECT_0)
	status = 0;
      else
	status = 1;
    }
  return status;
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t *mutex)
{
  int status = 0;

  if (__gthread_active_p ())
    {
      if (WaitForSingleObject (*mutex, 0) == WAIT_OBJECT_0)
	status = 0;
      else
	status = 1;
    }
  return status;
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t *mutex)
{
  if (__gthread_active_p ())
    return (ReleaseMutex (*mutex) != 0) ? 0 : 1;
  else
    return 0;
}

#endif /* not __gthr_win32_h */

