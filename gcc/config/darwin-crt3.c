/* __cxa_atexit backwards-compatibility support for Darwin.
   Copyright (C) 2006 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* It is incorrect to include config.h here, because this file is being
   compiled for the target, and hence definitions concerning only the host
   do not apply.  */

#include "tconfig.h"
#include "tsystem.h"

#include <dlfcn.h>
#include <stdbool.h>
#include <stdlib.h>

/* This file works around two different problems.

   The first problem is that there is no __cxa_atexit on Mac OS versions
   before 10.4.  It fixes this by providing one, and having it called from
   a destructor.  This is not quite as good as having a real __cxa_atexit,
   but it's good enough to imitate the behaviour that you'd get if
   you didn't have one.

   The second problem is that on 10.4 Mac OS versions, __cxa_finalize
   doesn't work right: it doesn't run routines that were registered
   while other atexit routines are running.  This is worked around by
   installing our own handler so that it runs last, and repeatedly
   running __cxa_finalize until no new calls to __cxa_atexit are made.  */

typedef int (*cxa_atexit_p)(void (*func) (void*), void* arg, void* dso);

#ifdef __ppc__
void __cxa_finalize (void* dso) __attribute__((weak));
#else
void __cxa_finalize (void* dso);
#endif

/* new_atexit_routines is set if __cxa_finalize exists in the system C
   library and our copy of __cxa_atexit has been called.  */

static bool new_atexit_routines;

/* first_atexit_handler is called after all other atexit routines
   that were registered before __cxa_finalize is called.
   It may be called more than once, but is not re-entered.  */

static void
first_atexit_handler(void* dso)
{
  /* Keep running __cxa_finalize until no new atexit routines are
     registered.  
     Note that this means __cxa_finalize will be called at least twice,
     even if the first call didn't register any new routines.  */
  while (new_atexit_routines) {
    new_atexit_routines = false;
    __cxa_finalize (dso);
  };
}

/* This is our wrapper around __cxa_atexit that's called if __cxa_finalize
   exists in the system library.  All it does is, on its first call,
   install first_atexit_handler; and on every call, set new_atexit_routines
   and pass control to the system __cxa_atexit.
   This proves to be somewhat more complicated than you might expect,
   because it may be called in a multithreaded environment.  Fortunately
   it turns out to be possible to do what's needed without resorting
   to locking.  */

static int
cxa_atexit_wrapper (void (*func) (void*), void* arg, void* dso)
{
  static volatile cxa_atexit_p real_cxa_atexit;
  cxa_atexit_p auto_cxa_atexit = real_cxa_atexit;
  if (! auto_cxa_atexit)
    {
      void* handle = dlopen ("/usr/lib/libSystem.B.dylib", RTLD_NOLOAD);
      if (! handle)
	return -1;
      
      auto_cxa_atexit = (cxa_atexit_p)dlsym (handle, "__cxa_atexit");
      if (! auto_cxa_atexit)
	return -1;
    }
  /* At this point, auto_cxa_atexit contains the address of
     the system __cxa_atexit.  */
  if (! real_cxa_atexit)
    {
      /* Install our handler above before any other handlers
	 for this image, so it will be called last.  */
      int result = (*auto_cxa_atexit)(first_atexit_handler, dso, dso);
      if (result != 0)
	return result;
      /* Now set the global real_cxa_atexit to prevent further
	 installations of first_atexit_handler.  Do this after
	 the installation so that if another thread sees it is set,
	 it can be sure that first_atexit_handler really has been
	 installed.  */
      real_cxa_atexit = auto_cxa_atexit;
    }
  /* At this point, we know that first_atexit_handler has been
     installed at least once, and real_cxa_atexit is not NULL.  */
  /* It's not necessary to mark new_atexit_routines as volatile, so long
     as this write eventually happens before this shared object is
     unloaded.  */
  new_atexit_routines = true;
  /* Call the original __cxa_atexit for this function.  */
  return (*auto_cxa_atexit)(func, arg, dso);
}

#ifdef __ppc__
/* This code is used while running on 10.3.9, when __cxa_atexit doesn't
   exist in the system library.  10.3.9 only supported regular PowerPC,
   so this code isn't necessary on x86 or ppc64.  */

/* This structure holds a routine to call.  */
struct atexit_routine
{
  struct atexit_routine * next;
  void (*func)(void *);
  void * arg;
};

static struct atexit_routine * volatile atexit_routines_list;

/* If __cxa_atexit doesn't exist at all in the system library, this
   routine is used; it completely emulates __cxa_atexit.  

   This routine has to be thread-safe, but fortunately this just means
   that it has to do atomic list insertion.  */

static int
cxa_atexit_substitute (void (*func) (void*), void* arg,
		       /* The 'dso' value will always be equal to this
			  object's __dso_handle.  */
		       void* dso __attribute__((unused)))
{
  struct atexit_routine * s = malloc (sizeof (struct atexit_routine));
  struct atexit_routine * next, * old_next;
  if (!s)
    return -1;
  s->func = func;
  s->arg = arg;
  next = atexit_routines_list;
  do {
    s->next = old_next = next;
    next = __sync_val_compare_and_swap (&atexit_routines_list, old_next, s);
  } while (next != old_next);
  return 0;
}

/* The routines added in cxa_atexit_substitute get run here, in a destructor.
   This routine doesn't have to be thread-safe.  */

static void cxa_dtor (void) __attribute__((destructor));
static void
cxa_dtor (void)
{
  while (atexit_routines_list)
    {
      struct atexit_routine * working_list = atexit_routines_list;
      atexit_routines_list = NULL;
      while (working_list)
	{
	  struct atexit_routine * called_routine = working_list;
	  working_list->func (working_list->arg);
	  working_list = working_list->next;
	  free (called_routine);
	}
    }
}
#endif

int __cxa_atexit (void (*func) (void*), void* arg, 
		  void* dso) __attribute__((visibility("hidden")));
int
__cxa_atexit (void (*func) (void*), void* arg, void* dso)
{
#ifdef __ppc__
  if (! __cxa_finalize)
    return cxa_atexit_substitute (func, arg, dso);
#endif
  return cxa_atexit_wrapper (func, arg, dso);
}
