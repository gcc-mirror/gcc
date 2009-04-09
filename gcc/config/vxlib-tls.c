/* Copyright (C) 2002, 2003, 2004, 2005, 2009 Free Software Foundation, Inc.
   Contributed by Zack Weinberg <zack@codesourcery.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Threads compatibility routines for libgcc2 for VxWorks.
   These are out-of-line routines called from gthr-vxworks.h. 

   This file provides the TLS related support routines, calling specific
   VxWorks kernel entry points for this purpose.  The base VxWorks 5.x kernels
   don't feature these entry points, and we provide gthr_supp_vxw_5x.c as an
   option to fill this gap.  Asking users to rebuild a kernel is not to be
   taken lightly, still, so we have isolated these routines from the rest of
   vxlib to ensure that the kernel dependencies are only dragged when really
   necessary.  */

#include "tconfig.h"
#include "tsystem.h"
#include "gthr.h"

#if defined(__GTHREADS)
#include <vxWorks.h>
#ifndef __RTP__
#include <vxLib.h>
#endif
#include <taskLib.h>
#ifndef __RTP__
#include <taskHookLib.h>
#else
# include <errno.h>
#endif

/* Thread-local storage.

   We reserve a field in the TCB to point to a dynamically allocated
   array which is used to store TLS values.  A TLS key is simply an
   offset in this array.  The exact location of the TCB field is not
   known to this code nor to vxlib.c -- all access to it indirects
   through the routines __gthread_get_tls_data and
   __gthread_set_tls_data, which are provided by the VxWorks kernel.

   There is also a global array which records which keys are valid and
   which have destructors.

   A task delete hook is installed to execute key destructors.  The
   routines __gthread_enter_tls_dtor_context and
   __gthread_leave_tls_dtor_context, which are also provided by the
   kernel, ensure that it is safe to call free() on memory allocated
   by the task being deleted.  (This is a no-op on VxWorks 5, but
   a major undertaking on AE.)

   The task delete hook is only installed when at least one thread
   has TLS data.  This is a necessary precaution, to allow this module
   to be unloaded - a module with a hook can not be removed.

   Since this interface is used to allocate only a small number of
   keys, the table size is small and static, which simplifies the
   code quite a bit.  Revisit this if and when it becomes necessary.  */

#define MAX_KEYS 4

/* This is the structure pointed to by the pointer returned
   by __gthread_get_tls_data.  */
struct tls_data
{
  int *owner;
  void *values[MAX_KEYS];
  unsigned int generation[MAX_KEYS];
};

/* To make sure we only delete TLS data associated with this object,
   include a pointer to a local variable in the TLS data object.  */
static int self_owner;

/* The number of threads for this module which have active TLS data.
   This is protected by tls_lock.  */
static int active_tls_threads;

/* kernel provided routines */
extern void *__gthread_get_tls_data (void);
extern void __gthread_set_tls_data (void *data);

extern void __gthread_enter_tls_dtor_context (void);
extern void __gthread_leave_tls_dtor_context (void);


/* This is a global structure which records all of the active keys.

   A key is potentially valid (i.e. has been handed out by
   __gthread_key_create) iff its generation count in this structure is
   even.  In that case, the matching entry in the dtors array is a
   routine to be called when a thread terminates with a valid,
   non-NULL specific value for that key.

   A key is actually valid in a thread T iff the generation count
   stored in this structure is equal to the generation count stored in
   T's specific-value structure.  */

typedef void (*tls_dtor) (void *);

struct tls_keys
{
  tls_dtor dtor[MAX_KEYS];
  unsigned int generation[MAX_KEYS];
};

#define KEY_VALID_P(key) !(tls_keys.generation[key] & 1)

/* Note: if MAX_KEYS is increased, this initializer must be updated
   to match.  All the generation counts begin at 1, which means no
   key is valid.  */
static struct tls_keys tls_keys =
{
  { 0, 0, 0, 0 },
  { 1, 1, 1, 1 }
};

/* This lock protects the tls_keys structure.  */
static __gthread_mutex_t tls_lock;

static __gthread_once_t tls_init_guard = __GTHREAD_ONCE_INIT;

/* Internal routines.  */

/* The task TCB has just been deleted.  Call the destructor
   function for each TLS key that has both a destructor and
   a non-NULL specific value in this thread.

   This routine does not need to take tls_lock; the generation
   count protects us from calling a stale destructor.  It does
   need to read tls_keys.dtor[key] atomically.  */

static void
tls_delete_hook (void *tcb ATTRIBUTE_UNUSED)
{
  struct tls_data *data;
  __gthread_key_t key;

#ifdef __RTP__
  data = __gthread_get_tls_data ();
#else
  /* In kernel mode, we can be called in the context of the thread
     doing the killing, so must use the TCB to determine the data of
     the thread being killed.  */
  data = __gthread_get_tsd_data (tcb);
#endif
  
  if (data && data->owner == &self_owner)
    {
      __gthread_enter_tls_dtor_context ();
      for (key = 0; key < MAX_KEYS; key++)
	{
	  if (data->generation[key] == tls_keys.generation[key])
	    {
	      tls_dtor dtor = tls_keys.dtor[key];

	      if (dtor)
		dtor (data->values[key]);
	    }
	}
      free (data);

      /* We can't handle an error here, so just leave the thread
	 marked as loaded if one occurs.  */
      if (__gthread_mutex_lock (&tls_lock) != ERROR)
	{
	  active_tls_threads--;
	  if (active_tls_threads == 0)
	    taskDeleteHookDelete ((FUNCPTR)tls_delete_hook);
	  __gthread_mutex_unlock (&tls_lock);
	}
#ifdef __RTP__
      __gthread_set_tls_data (0);
#else
      __gthread_set_tsd_data (tcb, 0);
#endif
      __gthread_leave_tls_dtor_context ();
    }
} 

/* Initialize global data used by the TLS system.  */
static void
tls_init (void)
{
  __GTHREAD_MUTEX_INIT_FUNCTION (&tls_lock);
}

static void tls_destructor (void) __attribute__ ((destructor));
static void
tls_destructor (void)
{
#ifdef __RTP__
  /* All threads but this one should have exited by now.  */
  tls_delete_hook (NULL);
#else
  /* Unregister the hook forcibly.  The counter of active threads may
     be incorrect, because constructors (like the C++ library's) and
     destructors (like this one) run in the context of the shell rather
     than in a task spawned from this module.  */
  taskDeleteHookDelete ((FUNCPTR)tls_delete_hook);
#endif

  if (tls_init_guard.done && __gthread_mutex_lock (&tls_lock) != ERROR)
    semDelete (tls_lock);
}

/* External interface */

/* Store in KEYP a value which can be passed to __gthread_setspecific/
   __gthread_getspecific to store and retrieve a value which is
   specific to each calling thread.  If DTOR is not NULL, it will be
   called when a thread terminates with a non-NULL specific value for
   this key, with the value as its sole argument.  */

int
__gthread_key_create (__gthread_key_t *keyp, tls_dtor dtor)
{
  __gthread_key_t key;

  __gthread_once (&tls_init_guard, tls_init);

  if (__gthread_mutex_lock (&tls_lock) == ERROR)
    return errno;

  for (key = 0; key < MAX_KEYS; key++)
    if (!KEY_VALID_P (key))
      goto found_slot;

  /* no room */
  __gthread_mutex_unlock (&tls_lock);
  return EAGAIN;

 found_slot:
  tls_keys.generation[key]++;  /* making it even */
  tls_keys.dtor[key] = dtor;
  *keyp = key;
  __gthread_mutex_unlock (&tls_lock);
  return 0;
}

/* Invalidate KEY; it can no longer be used as an argument to
   setspecific/getspecific.  Note that this does NOT call destructor
   functions for any live values for this key.  */
int
__gthread_key_delete (__gthread_key_t key)
{
  if (key >= MAX_KEYS)
    return EINVAL;

  __gthread_once (&tls_init_guard, tls_init);

  if (__gthread_mutex_lock (&tls_lock) == ERROR)
    return errno;

  if (!KEY_VALID_P (key))
    {
      __gthread_mutex_unlock (&tls_lock);
      return EINVAL;
    }

  tls_keys.generation[key]++;  /* making it odd */
  tls_keys.dtor[key] = 0;

  __gthread_mutex_unlock (&tls_lock);
  return 0;
}

/* Retrieve the thread-specific value for KEY.  If it has never been
   set in this thread, or KEY is invalid, returns NULL.

   It does not matter if this function races with key_create or
   key_delete; the worst that can happen is you get a value other than
   the one that a serialized implementation would have provided.  */

void *
__gthread_getspecific (__gthread_key_t key)
{
  struct tls_data *data;

  if (key >= MAX_KEYS)
    return 0;

  data = __gthread_get_tls_data ();

  if (!data)
    return 0;

  if (data->generation[key] != tls_keys.generation[key])
    return 0;

  return data->values[key];
}

/* Set the thread-specific value for KEY.  If KEY is invalid, or
   memory allocation fails, returns -1, otherwise 0.

   The generation count protects this function against races with
   key_create/key_delete; the worst thing that can happen is that a
   value is successfully stored into a dead generation (and then
   immediately becomes invalid).  However, we do have to make sure
   to read tls_keys.generation[key] atomically.  */

int
__gthread_setspecific (__gthread_key_t key, void *value)
{
  struct tls_data *data;
  unsigned int generation;

  if (key >= MAX_KEYS)
    return EINVAL;

  data = __gthread_get_tls_data ();
  if (!data)
    {
      if (__gthread_mutex_lock (&tls_lock) == ERROR)
	return ENOMEM;
      if (active_tls_threads == 0)
	taskDeleteHookAdd ((FUNCPTR)tls_delete_hook);
      active_tls_threads++;
      __gthread_mutex_unlock (&tls_lock);

      data = malloc (sizeof (struct tls_data));
      if (!data)
	return ENOMEM;

      memset (data, 0, sizeof (struct tls_data));
      data->owner = &self_owner;
      __gthread_set_tls_data (data);
    }

  generation = tls_keys.generation[key];

  if (generation & 1)
    return EINVAL;

  data->generation[key] = generation;
  data->values[key] = value;

  return 0;
}
#endif /* __GTHREADS */
