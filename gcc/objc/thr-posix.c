/* GNU Objective C Runtime Thread Interface for POSIX compliant threads
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Galen C. Hunt (gchunt@cs.rochester.edu)
   Modified for Linux/Pthreads by Kai-Uwe Sattler (kus@iti.cs.uni-magdeburg.de)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.

GNU CC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

#include <objc/thr.h>
#include "runtime.h"
#include <pthread.h>

/********
 *  This structure represents a single mutual exclusion lock.  Lock semantics
 *  are detailed with the subsequent functions.  We use whatever lock is
 *  provided by the system.  We augment it with depth and current owner id
 *  fields to implement and re-entrant lock.
 */
struct objc_mutex 
{
    volatile objc_thread_t     owner;          /* Id of thread that owns.  */
    volatile int                depth;          /* # of acquires.           */
    pthread_mutex_t             lock;           /* pthread mutex.           */
};

/*****************************************************************************
 *  Static variables.
 */
static pthread_key_t    __objc_thread_data_key; /* Data key for thread data.*/


/********
 *  Initialize the threads subsystem.  Returns 0 if successful, or -1 if no
 *  thread support is available.
 */
int
__objc_init_thread_system(void)
{
    if (pthread_key_create(&__objc_thread_data_key, NULL) == 0)
        return 0;                               /* Yes, return success.     */
    
    return -1;                                  /* Failed.                  */
}

int
__objc_fini_thread_system(void)
{
  return 0;
}

/********
 *  Create a new thread of execution and return its id.  Return NULL if fails.
 *  The new thread starts in "func" with the given argument.
 */
objc_thread_t
objc_thread_create(void (*func)(void *arg), void *arg)
{
    objc_thread_t      thread_id = NULL;       /* Detached thread id.      */
    pthread_t           new_thread_handle;      /* DCE thread handle.       */

    objc_mutex_lock(__objc_runtime_mutex);

    if (pthread_create(&new_thread_handle, NULL,
                       (void *)func, arg) == 0) {
        thread_id = (objc_thread_t) new_thread_handle;
        pthread_detach(new_thread_handle);     /* Fully detach thread.     */
	__objc_runtime_threads_alive++;
    }
    
    objc_mutex_unlock(__objc_runtime_mutex);
    return thread_id;
}

/********
 *  Set the current thread's priority.
 */
int
objc_thread_set_priority(int priority)
{
#if 0 /* no get/set priority in Linux pthreads */

    int         sys_priority = 0;

    switch (priority) {
    case OBJC_THREAD_INTERACTIVE_PRIORITY:
        sys_priority = (PRI_FG_MIN_NP + PRI_FG_MAX_NP) / 2;
        break;
    default:
    case OBJC_THREAD_BACKGROUND_PRIORITY:
        sys_priority = (PRI_BG_MIN_NP + PRI_BG_MAX_NP) / 2;
        break;
    case OBJC_THREAD_LOW_PRIORITY:
        sys_priority = (PRI_BG_MIN_NP + PRI_BG_MAX_NP) / 2;
        break;
    }
    
    if (pthread_setprio(pthread_self(), sys_priority) >= 0)
        return 0;                               /* Changed priority. End.   */
    
#endif
    return -1;                                  /* Failed.                  */
}

/********
 *  Return the current thread's priority.
 */
int
objc_thread_get_priority(void)
{
#if 0 /* no get/set priority in Linux pthreads */
    int         sys_priority;                   /* DCE thread priority.     */
    
    if ((sys_priority = pthread_getprio(pthread_self())) >= 0) {
        if (sys_priority >= PRI_FG_MIN_NP && sys_priority <= PRI_FG_MAX_NP)
            return OBJC_THREAD_INTERACTIVE_PRIORITY;
        if (sys_priority >= PRI_BG_MIN_NP && sys_priority <= PRI_BG_MAX_NP)
            return OBJC_THREAD_BACKGROUND_PRIORITY;
        return OBJC_THREAD_LOW_PRIORITY;
    }
#endif
    return -1;                                  /* Couldn't get priority.   */
}

/********
 *  Yield our process time to another thread.  Any BUSY waiting that is done
 *  by a thread should use this function to make sure that other threads can
 *  make progress even on a lazy uniprocessor system.
 */
void
objc_thread_yield(void)
{
    pthread_yield();                            /* Yield to equal thread.   */
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
      
  pthread_exit(&__objc_thread_exit_status);     /* Terminate thread.        */
  return -1;
}

/********
 *  Returns an integer value which uniquely describes a thread.  Must not be
 *  -1 which is reserved as a marker for "no thread".
 */
objc_thread_t
objc_thread_id(void)
{
  pthread_t self = pthread_self();

  return (objc_thread_t) self;               /* Return thread handle.    */
}

/********
 *  Sets the thread's local storage pointer.  Returns 0 if successful or -1
 *  if failed.
 */
int
objc_thread_set_data(void *value)
{
    if (pthread_setspecific(__objc_thread_data_key, (void *)value) == 0)
        return 0;                           	/* Return thread data.      */
    return -1;
}

/********
 *  Returns the thread's local storage pointer.  Returns NULL on failure.
 */
void *
objc_thread_get_data(void)
{
    return pthread_getspecific(__objc_thread_data_key);
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

    err = pthread_mutex_init(&mutex->lock, NULL);
    
    if (err != 0) {                             /* System init failed?      */
        objc_free(mutex);                       /* Yes, free local memory.  */
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
    
    pthread_mutex_unlock(&mutex->lock);         /* Must unlock system mutex.*/
    pthread_mutex_destroy(&mutex->lock);        /* Free system mutex.       */
    
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
    objc_thread_t     thread_id;                /* Cache our thread id. */

    if (!mutex)                                 /* Is argument bad?         */
        return -1;                              /* Yes, abort.              */
    thread_id = objc_thread_id();               /* Get this thread's id.    */
    if (mutex->owner == thread_id)              /* Already own lock?        */
        return ++mutex->depth;                  /* Yes, increment depth.    */

    if (pthread_mutex_lock(&mutex->lock) != 0)  /* Lock DCE system mutex.   */
        return -1;                              /* Failed, abort.           */
    
    mutex->owner = thread_id;                   /* Mark thread as owner.    */
    return mutex->depth = 1;                    /* Increment depth to end.  */
}

/********
 *  Try to grab a lock on a mutex.  If this thread already has a lock on
 *  this mutex then we increment the lock count and return it.  If another
 *  thread has a lock on the mutex returns -1.
 */
int
objc_mutex_trylock(objc_mutex_t mutex)
{
    objc_thread_t    thread_id;                 /* Cache our thread id. */

    if (!mutex)                                 /* Is argument bad?         */
        return -1;                              /* Yes, abort.              */
    thread_id = objc_thread_id();               /* Get this thread's id.    */
    if (mutex->owner == thread_id)              /* Already own lock?        */
        return ++mutex->depth;                  /* Yes, increment depth.    */
    
    if (pthread_mutex_trylock(&mutex->lock) != 1) /* Lock DCE system mutex. */
        return -1;                              /* Failed, abort.           */
    
    mutex->owner = thread_id;                   /* Mark thread as owner.    */
    return mutex->depth = 1;                    /* Increment depth to end.  */
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
    objc_thread_t   thread_id;                 /* Cache our thread id.     */
    
    if (!mutex)                                 /* Is argument bad?         */
        return -1;                              /* Yes, abort.              */
    thread_id = objc_thread_id();               /* Get this thread's id.    */
    if (mutex->owner != thread_id)              /* Does some else own lock? */
        return -1;                              /* Yes, abort.              */
    if (mutex->depth > 1)                       /* Released last lock?      */
        return --mutex->depth;                  /* No, Decrement depth, end.*/
    mutex->depth = 0;                           /* Yes, reset depth to 0.   */
    mutex->owner = NULL;                        /* Set owner to "no thread".*/
    
    if (pthread_mutex_unlock(&mutex->lock) != 0)  /* Unlock system mutex.   */
        return -1;                              /* Failed, abort.           */
    
    return 0;                                   /* No, return success.      */
}
