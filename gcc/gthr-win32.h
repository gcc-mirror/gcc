/* Threads compatibility routines for libgcc2.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1999, 2000 Free Software Foundation, Inc.
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

   1. The destructor supplied to __gthread_key_create is ignored for
      generic x86-win32 ports. This will certainly cause memory leaks 
      due to unreclaimed eh contexts (sizeof (eh_context) is at least 
      24 bytes for x86 currently).

      This memory leak may be significant for long-running applications
      that make heavy use of C++ EH.

      However, Mingw runtime (version 0.3 or newer) provides a mechanism
      to emulate pthreads key dtors; the runtime provides a special DLL,
      linked in if -mthreads option is specified, that runs the dtors in
      the reverse order of registration when each thread exits. If
      -mthreads option is not given, a stub is linked in instead of the
      DLL, which results in memory leak. Other x86-win32 ports can use 
      the same technique of course to avoid the leak.

   2. The error codes returned are non-POSIX like, and cast into ints.
      This may cause incorrect error return due to truncation values on 
      hw where sizeof (DWORD) > sizeof (int).
   
   3. We might consider using Critical Sections instead of Windows32 
      mutexes for better performance, but emulating __gthread_mutex_trylock 
      interface becomes more complicated (Win9x does not support
      TryEnterCriticalSectioni, while NT does).
  
   The basic framework should work well enough. In the long term, GCC
   needs to use Structured Exception Handling on Windows32.  */

#define __GTHREADS 1

#include <windows.h>
#include <errno.h>
#ifdef __MINGW32__
#include <_mingw.h>
#endif

typedef DWORD __gthread_key_t;

typedef struct {
  int done;
  long started;
} __gthread_once_t;

typedef HANDLE __gthread_mutex_t;

#define __GTHREAD_ONCE_INIT {FALSE, -1}
#define __GTHREAD_MUTEX_INIT_FUNCTION __gthread_mutex_init_function

#if __MINGW32_MAJOR_VERSION >= 1 || \
  (__MINGW32_MAJOR_VERSION == 0 && __MINGW32_MINOR_VERSION > 2)
#define MINGW32_SUPPORTS_MT_EH 1
extern int __mingwthr_key_dtor PARAMS ((DWORD, void (*) (void *)));
/* Mingw runtime >= v0.3 provides a magic variable that is set to non-zero
   if -mthreads option was specified, or 0 otherwise. This is to get around 
   the lack of weak symbols in PE-COFF.  */
extern int _CRT_MT;
#endif

static inline int
__gthread_active_p ()
{
#ifdef MINGW32_SUPPORTS_MT_EH
  return _CRT_MT;
#else
  return 1;
#endif
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
      else
	{
	  /* Another thread is currently executing the code, so wait for it 
	     to finish; yield the CPU in the meantime.  If performance 
	     does become an issue, the solution is to use an Event that 
	     we wait on here (and set above), but that implies a place to 
	     create the event before this routine is called.  */ 
	  while (! once->done)
	    Sleep (0);
	}
    }
  
  return 0;
}

/* Windows32 thread local keys don't support destructors; this leads to
   leaks, especially in threaded applications making extensive use of 
   C++ EH. Mingw uses a thread-support DLL to work-around this problem.  */
static inline int
__gthread_key_create (__gthread_key_t *key, void (*dtor) (void *))
{
  int status = 0;
  DWORD tls_index = TlsAlloc ();
  if (tls_index != 0xFFFFFFFF)
    {
      *key = tls_index;
#ifdef MINGW32_SUPPORTS_MT_EH
      /* Mingw runtime will run the dtors in reverse order for each thread
         when the thread exits.  */
      status = __mingwthr_key_dtor (*key, dtor);
#endif
    }
  else
    status = (int) GetLastError ();
  return status;
}

/* Currently, this routine is called only for Mingw runtime, and if
   -mthreads option is chosen to link in the thread support DLL.  */ 
static inline int
__gthread_key_dtor (__gthread_key_t key, void *ptr)
{
  /* Nothing needed. */
  return 0;
}

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
  return (TlsSetValue (key, (void*) ptr) != 0) ? 0 : (int) GetLastError ();
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

