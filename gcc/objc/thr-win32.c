/* GNU Objective C Runtime Thread Interface - Win32 Implementation
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Galen C. Hunt (gchunt@cs.rochester.edu)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.

GNU CC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
GNU CC; see the file COPYING.  If not, write to the Free Software
Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

#include <objc/thr.h>
#include "runtime.h"

#ifndef __OBJC__
#define __OBJC__
#endif
#include <windows.h>

/********
 *  This structure represents a single mutual exclusion lock.  Lock semantics
 *  are detailed with the subsequent functions.  We use whatever lock is
 *  provided by the system.  We augment it with depth and current owner id
 *  fields to implement and re-entrant lock.
 */
struct objc_mutex 
{
  volatile objc_thread_t       owner;        	/* Id of thread that owns.  */
  volatile int                  depth;          /* # of acquires.           */
  HANDLE                        handle;         /* Win32 mutex HANDLE.      */
};

/*****************************************************************************
 *  Static variables.
 */
static DWORD	__objc_data_tls = (DWORD)-1;	/* Win32 Thread Local Index.*/

/********
 *  Initialize the threads subsystem.  Returns 0 if successful, or -1 if no
 *  thread support is available.
 */
int
__objc_init_thread_system(void)
{
  DEBUG_PRINTF("__objc_init_thread_system\n");

  if ((__objc_data_tls = TlsAlloc()) != (DWORD)-1)
    return 0;                               	/* Yes, return success.     */
    
  return -1;                                  	/* Failed.                  */
}

int
__objc_fini_thread_system(void)
{
  if (__objc_data_tls != (DWORD)-1) {
    TlsFree(__objc_data_tls);
    return 0;
  }
  return -1;
}

/********
 *  Create a new thread of execution and return its id.  Return NULL if fails.
 *  The new thread starts in "func" with the given argument.
 */
objc_thread_t
objc_thread_create(void (*func)(void *arg), void *arg)
{
  DWORD        	thread_id = 0;                  /* Detached thread id.      */
  HANDLE	win32_handle;			/* Win32 thread handle.     */

  objc_mutex_lock(__objc_runtime_mutex);
  
  if ((win32_handle = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)func,
                                   arg, 0, &thread_id))) {
      __objc_runtime_threads_alive++;
  }
  else
      thread_id = 0;
  
  objc_mutex_unlock(__objc_runtime_mutex);
  
  return (objc_thread_t)thread_id;
}

/********
 *  Set the current thread's priority.
 */
int
objc_thread_set_priority(int priority)
{
  int         	sys_priority = 0;

  switch (priority) {
  case OBJC_THREAD_INTERACTIVE_PRIORITY:
    sys_priority = THREAD_PRIORITY_NORMAL;
    break;
  default:
  case OBJC_THREAD_BACKGROUND_PRIORITY:
    sys_priority = THREAD_PRIORITY_BELOW_NORMAL;
    break;
  case OBJC_THREAD_LOW_PRIORITY:
    sys_priority = THREAD_PRIORITY_LOWEST;
    break;
  }
  if (SetThreadPriority(GetCurrentThread(), sys_priority))
    return 0;                                  	/* Changed priority. End.   */
    
  return -1;                                  	/* Failed.                  */
}

/********
 *  Return the current thread's priority.
 */
int
objc_thread_get_priority(void)
{
  int         	sys_priority;

  sys_priority = GetThreadPriority(GetCurrentThread());
  
  switch (sys_priority) {
  case THREAD_PRIORITY_HIGHEST:
  case THREAD_PRIORITY_TIME_CRITICAL:
  case THREAD_PRIORITY_ABOVE_NORMAL:
  case THREAD_PRIORITY_NORMAL:
    return OBJC_THREAD_INTERACTIVE_PRIORITY;

  default:
  case THREAD_PRIORITY_BELOW_NORMAL:
    return OBJC_THREAD_BACKGROUND_PRIORITY;
    
  case THREAD_PRIORITY_IDLE:
  case THREAD_PRIORITY_LOWEST:
    return OBJC_THREAD_LOW_PRIORITY;
  }
  return -1;                                  	/* Couldn't get priority.   */
}

/********
 *  Yield our process time to another thread.  Any BUSY waiting that is done
 *  by a thread should use this function to make sure that other threads can
 *  make progress even on a lazy uniprocessor system.
 */
void
objc_thread_yield(void)
{
  Sleep(0);	                            	/* Yield to equal thread.   */
}

/********
 *  Terminate the current tread.  Doesn't return anything.  Doesn't return.
 *  Actually, if it failed returns -1.
 */
int
objc_thread_exit(void)
{
  objc_mutex_lock(__objc_runtime_mutex);
  __objc_runtime_threads_alive--;
  objc_mutex_unlock(__objc_runtime_mutex);
  
  ExitThread(__objc_thread_exit_status);   	/* Terminate thread.        */
  return -1;
}

/********
 *  Returns an integer value which uniquely describes a thread.  Must not be
 *  -1 which is reserved as a marker for "no thread".
 */
objc_thread_t
objc_thread_id(void)
{
  return (objc_thread_t)GetCurrentThreadId();  /* Return thread id.        */
}

/********
 *  Sets the thread's local storage pointer.  Returns 0 if successful or -1
 *  if failed.
 */
int
objc_thread_set_data(void *value)
{
  if (TlsSetValue(__objc_data_tls, value))
    return 0;                           	/* Return thread data.      */
  return -1;
}

/********
 *  Returns the thread's local storage pointer.  Returns NULL on failure.
 */
void *
objc_thread_get_data(void)
{
  return TlsGetValue(__objc_data_tls);          /* Return thread data.      */
}

/********
 *  Allocate a mutex.  Return the mutex pointer if successful or NULL if
 *  the allocation fails for any reason.
 */
objc_mutex_t
objc_mutex_allocate(void)
{
    objc_mutex_t mutex;
    int         err = 0;

    if (!(mutex = (objc_mutex_t)objc_malloc(sizeof(struct objc_mutex))))
        return NULL;                            /* Abort if malloc failed.  */

    if ((mutex->handle = CreateMutex(NULL, 0, NULL)) == NULL) {
        objc_free(mutex);                       /* Failed, free memory.     */
        return NULL;                            /* Abort.                   */
    }
    mutex->owner = NULL;                        /* No owner.                */
    mutex->depth = 0;                           /* No locks.                */
    return mutex;                               /* Return mutex handle.     */
}

/********
 *  Deallocate a mutex.  Note that this includes an implicit mutex_lock to
 *  insure that no one else is using the lock.  It is legal to deallocate
 *  a lock if we have a lock on it, but illegal to deallotcate a lock held
 *  by anyone else.
 *  Returns the number of locks on the thread.  (1 for deallocate).
 */
int
objc_mutex_deallocate(objc_mutex_t mutex)
{
    int         depth;                          /* # of locks on mutex.     */

    if (!mutex)                                 /* Is argument bad?         */
        return -1;                              /* Yes, abort.              */
    depth = objc_mutex_lock(mutex);             /* Must have lock.          */

    CloseHandle(mutex->handle);			/* Close Win32 handle.      */
    
    objc_free(mutex);                           /* Free memory.             */
    return depth;                               /* Return last depth.       */
}

/********
 *  Grab a lock on a mutex.  If this thread already has a lock on this mutex
 *  then we increment the lock count.  If another thread has a lock on the 
 *  mutex we block and wait for the thread to release the lock.
 *  Returns the lock count on the mutex held by this thread.
 */
int
objc_mutex_lock(objc_mutex_t mutex)
{
    objc_thread_t      thread_id;              /* Cache our thread id.     */
    int                 status;

    if (!mutex)                                 /* Is argument bad?         */
        return -1;                              /* Yes, abort.              */
    thread_id = objc_thread_id();               /* Get this thread's id.    */
    if (mutex->owner == thread_id)              /* Already own lock?        */
        return ++mutex->depth;                  /* Yes, increment depth.    */

    status = WaitForSingleObject(mutex->handle, INFINITE);
    if (status != WAIT_OBJECT_0 && status != WAIT_ABANDONED)
        return -1;                              /* Failed, abort.           */
    
    mutex->owner = thread_id;                   /* Mark thread as owner.    */

    return ++mutex->depth;                      /* Increment depth to end.  */
}

/********
 *  Try to grab a lock on a mutex.  If this thread already has a lock on
 *  this mutex then we increment the lock count and return it.  If another
 *  thread has a lock on the mutex returns -1.
 */
int
objc_mutex_trylock(objc_mutex_t mutex)
{
    objc_thread_t      thread_id;              /* Cache our thread id.     */
    DWORD               status;                 /* Return status from Win32.*/

    if (!mutex)                                 /* Is argument bad?         */
        return -1;                              /* Yes, abort.              */
    thread_id = objc_thread_id();               /* Get this thread's id.    */
    if (mutex->owner == thread_id)              /* Already own lock?        */
        return ++mutex->depth;                  /* Yes, increment depth.    */

    status = WaitForSingleObject(mutex->handle, 0);
    if (status != WAIT_OBJECT_0 && status != WAIT_ABANDONED)
        return -1;                              /* Failed, abort.           */
    
    mutex->owner = thread_id;                   /* Mark thread as owner.    */
    return ++mutex->depth;                      /* Increment depth to end.  */
}

/********
 *  Decrements the lock count on this mutex by one.  If the lock count reaches
 *  zero, release the lock on the mutex.  Returns the lock count on the mutex.
 *  It is an error to attempt to unlock a mutex which this thread doesn't hold
 *  in which case return -1 and the mutex is unaffected.
 *  Will also return -1 if the mutex free fails.
 */
int
objc_mutex_unlock(objc_mutex_t mutex)
{
    objc_thread_t      thread_id;              /* Cache our thread id.     */
    
    if (!mutex)                                 /* Is argument bad?         */
        return -1;                              /* Yes, abort.              */
    thread_id = objc_thread_id();               /* Get this thread's id.    */
    if (mutex->owner != thread_id)              /* Does some else own lock? */
        return -1;                              /* Yes, abort.              */
    if (mutex->depth > 1)                       /* Released last lock?      */
        return --mutex->depth;                  /* No, Decrement depth, end.*/
    mutex->depth = 0;                           /* Yes, reset depth to 0.   */
    mutex->owner = NULL;                        /* Set owner to "no thread".*/
    
    if (ReleaseMutex(mutex->handle) == 0)
        return -1;                              /* Failed, abort.           */
    
    return 0;                                   /* No, return success.      */
}

/* End of File */
