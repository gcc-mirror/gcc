/* Catch and clean up data allocated in TLS.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
   2009 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This part is based on the implementation of Mumit Khan  <khan@nanotech.wisc.edu>
 * provided to mingw under public domain and ported for libgcc by Kai Tietz.
 */

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#include <stdlib.h>

/* The list of threads active with key/dtor pairs. */
typedef struct __mingwthr_key {
  DWORD key;
  void (*dtor) (void *);
  struct __mingwthr_key *next;
} __mingwthr_key_t;

#if defined(_WIN32) && !defined(__CYGWIN__)

/* Static functions for libgcc.  */
#ifdef SHARED
__declspec(dllexport)
int _CRT_MT = 1;

/* Shared functions for libgcc.  */

/* Prototypes.  */
__declspec(dllexport) int __mingwthr_key_dtor (DWORD key, void (*) (void *));
__declspec(dllexport) int __mingwthr_remove_key_dtor (DWORD);
BOOL APIENTRY DllMain (HANDLE, DWORD, LPVOID);


/* To protect the thread/key association data structure modifications. */
static CRITICAL_SECTION __mingwthr_cs;
static __mingwthr_key_t *key_dtor_list;

/*
 * __mingwthr_key_add:
 *
 * Add key/dtor association for this thread. If the thread entry does not
 * exist, create a new one and add to the head of the threads list; add
 * the new assoc at the head of the keys list. 
 *
 */

static int
___mingwthr_add_key_dtor (DWORD key, void (*dtor) (void *))
{
  __mingwthr_key_t *new_key;

  new_key = (__mingwthr_key_t *) calloc (1, sizeof (__mingwthr_key_t));
  if (new_key == NULL)
    return -1;
  
  new_key->key = key;
  new_key->dtor = dtor;

  EnterCriticalSection (&__mingwthr_cs);

  new_key->next = key_dtor_list;
  key_dtor_list = new_key;

  LeaveCriticalSection (&__mingwthr_cs);

  return 0;
}

static int
___mingwthr_remove_key_dtor (DWORD key)
{
  __mingwthr_key_t *prev_key;
  __mingwthr_key_t *cur_key;

  EnterCriticalSection (&__mingwthr_cs);

  prev_key = NULL;
  cur_key = key_dtor_list;

  while (cur_key != NULL)
    {
       if( cur_key->key == key )
	 {
	    /* take key/dtor out of list */
	    if (prev_key == NULL)
	      key_dtor_list = cur_key->next;
	    else
	      prev_key->next = cur_key->next;

	    free (cur_key);
	    break;
	 }

       prev_key = cur_key;
       cur_key = cur_key->next;
    }

  LeaveCriticalSection (&__mingwthr_cs);

  return 0;
}

/*
 * __mingwthr_run_key_dtors (void):
 *
 * Callback from DllMain when thread detaches to clean up the key
 * storage. 
 *
 * Note that this does not delete the key itself, but just runs
 * the dtor if the current value are both non-NULL. Note that the
 * keys with NULL dtors are not added by __mingwthr_key_dtor, the
 * only public interface, so we don't need to check. 
 *
 */

static void
__mingwthr_run_key_dtors (void)
{
  __mingwthr_key_t *keyp;

  EnterCriticalSection (&__mingwthr_cs);

  for (keyp = key_dtor_list; keyp; )
    {
       LPVOID value = TlsGetValue (keyp->key);
       if (GetLastError () == ERROR_SUCCESS)
	 {
	    if (value)
	       (*keyp->dtor) (value);
	 }
       keyp = keyp->next;
    }
  
  LeaveCriticalSection (&__mingwthr_cs);
}

/*
 * __mingwthr_register_key_dtor (DWORD key, void (*dtor) (void *))
 *
 * Public interface called by C++ exception handling mechanism in
 * libgcc (cf: __gthread_key_create).
 *
 */

__declspec(dllexport)
int
__mingwthr_key_dtor (DWORD key, void (*dtor) (void *))
{
  if (dtor)
    return ___mingwthr_add_key_dtor (key, dtor);

  return 0;
}

__declspec(dllexport)
int
__mingwthr_remove_key_dtor (DWORD key)
{
   return ___mingwthr_remove_key_dtor (key);
}

BOOL APIENTRY
DllMain (HANDLE hDllHandle __attribute__ ((__unused__)),
	 DWORD reason /* Reason this function is being called. */,
	 LPVOID reserved __attribute__ ((__unused__)))
{
  switch (reason)
    {
    case DLL_PROCESS_ATTACH:
       InitializeCriticalSection (&__mingwthr_cs);
       break;

    case DLL_PROCESS_DETACH:
      __mingwthr_run_key_dtors ();
       DeleteCriticalSection (&__mingwthr_cs);
      break;

    case DLL_THREAD_ATTACH:
      break;

    case DLL_THREAD_DETACH:
      __mingwthr_run_key_dtors ();
      break;
    }
  return TRUE;
}
#endif
#endif
