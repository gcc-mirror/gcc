/* Thread and mutex controls for Objective C.
   Copyright (C) 1996 Free Software Foundation, Inc.
   Contributed by Galen C. Hunt (gchunt@cs.rochester.edu)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

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

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */


#ifndef __thread_INCLUDE_GNU
#define __thread_INCLUDE_GNU

#include "objc/objc.h"

/*************************************************************************
 *  Universal static variables:
 */
extern int __objc_thread_exit_status;      /* Global exit status.   */

/********
 *  Thread safe implementation types and functions.  
 */

#define OBJC_THREAD_INTERACTIVE_PRIORITY        2
#define OBJC_THREAD_BACKGROUND_PRIORITY         1
#define OBJC_THREAD_LOW_PRIORITY                0

typedef struct _objc_mutex *_objc_mutex_t;
typedef void * _objc_thread_t;

_objc_mutex_t objc_mutex_allocate(void);
int     objc_mutex_deallocate(_objc_mutex_t mutex);
int     objc_mutex_lock(_objc_mutex_t mutex);
int     objc_mutex_unlock(_objc_mutex_t mutex);
int     objc_mutex_trylock(_objc_mutex_t mutex);

_objc_thread_t objc_thread_create(void (*func)(void *arg), void *arg);
void    objc_thread_yield(void);
int     objc_thread_exit(void);
int     objc_thread_set_priority(int priority);
int     objc_thread_get_priority(void);
void *  objc_thread_get_data(void);
int     objc_thread_set_data(void *value);
_objc_thread_t objc_thread_id(void);

_objc_thread_t objc_thread_detach(SEL selector, id object, id argument);
int     objc_mutex_lock_x(_objc_mutex_t mutex, const char *f, int l);
int     objc_mutex_unlock_x(_objc_mutex_t mutex, const char *f, int l);

/* For debugging of locks, uncomment these two macros: */
/* #define objc_mutex_lock(x)      objc_mutex_lock_x(x, __FILE__, __LINE__) */
/* #define objc_mutex_unlock(x)    objc_mutex_unlock_x(x, __FILE__, __LINE__)*/

#endif /* not __thread_INCLUDE_GNU */
