/* GNU Objective C Runtime Thread Interface
   Copyright (C) 1996 Free Software Foundation, Inc.
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

#include <stdlib.h>
#include "runtime.h"

/*****************************************************************************
 *  Universal static variables:
 */
int     __objc_thread_exit_status = 0;          /* Global exit status.      */

/*****************************************************************************
 *  Universal Functionality
 */

/********
 *  First function called in a thread, starts everything else.
 */
struct __objc_thread_start_state
{
    SEL         selector;
    id          object;
    id          argument;
};

static volatile void
__objc_thread_detach_function(struct __objc_thread_start_state *istate)
{
    if (istate) {                               /* Is state valid?          */
        id      (*imp)(id,SEL,id);
        SEL     selector = istate->selector;
        id      object   = istate->object;
        id      argument = istate->argument;

        free(istate);

        if ((imp = (id(*)(id, SEL, id))objc_msg_lookup(object, selector))) {
            (*imp)(object, selector, argument);
        }
        else
            fprintf(stderr, "__objc_thread_start called with bad selector.\n");
    }
    else {
        fprintf(stderr, "__objc_thread_start called with NULL state.\n");
    }
    objc_thread_exit();
}

/********
 *  Detach a new thread of execution and return its id.  Returns NULL if fails.
 *  Thread is started by sending message with selector to object.  Message
 *  takes a single argument.
 */
_objc_thread_t
objc_thread_detach(SEL selector, id object, id argument)
{
  struct __objc_thread_start_state *istate;   /* Initialial thread state. */
  _objc_thread_t        thread_id = NULL;     /* Detached thread id.      */

  if (!(istate = (struct __objc_thread_start_state *)
	__objc_xmalloc(sizeof(*istate))))     /* Can we allocate state?   */
    return NULL;                              /* No, abort.               */

  istate->selector = selector;                /* Initialize the thread's  */
  istate->object = object;                    /*   state structure.       */
  istate->argument = argument;

  if ((thread_id = objc_thread_create((void *)__objc_thread_detach_function,
                                      istate)) == NULL) {
    free(istate);                           /* Release state if failed.   */
    return thread_id;
  }
  return thread_id;
}

#undef objc_mutex_lock()
#undef objc_mutex_unlock()

int
objc_mutex_unlock_x(_objc_mutex_t mutex, const char *f, int l)
{
    printf("%16.16s#%4d < unlock", f, l);
    return objc_mutex_unlock(mutex);
}

int
objc_mutex_lock_x(_objc_mutex_t mutex, const char *f, int l)
{
    printf("%16.16s#%4d < lock", f, l);
    return objc_mutex_lock(mutex);
}

/*****************************************************************************
 *  Implementation specific functionality:
 */

#if defined(__sparc__) && defined(__svr4__)     /* Solaris only code.       */
#include "thread-solaris.c"
#elif defined(__sgi__) && defined(__mips__)     /* IRIX only code.          */
#include "thread-irix.c"
#elif defined(__alpha__) && defined(__osf__)    /* Alpha OSF/1 only code.   */
#include "thread-decosf1.c"
#elif defined(__WIN32__)
#include "thread-win32.c"
#else						/* Single threaded code.    */
#include "thread-single.c"
#endif

/* End of File */
