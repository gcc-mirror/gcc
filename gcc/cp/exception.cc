// Functions for Exception Support for -*- C++ -*-
// Copyright (C) 1994, 95-97, 1998 Free Software Foundation

// This file is part of GNU CC.

// GNU CC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// GNU CC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with GNU CC; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA. 

// As a special exception, if you link this library with other files,
// some of which are compiled with GCC, to produce an executable,
// this library does not by itself cause the resulting executable
// to be covered by the GNU General Public License.
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.

#pragma implementation "exception"

#include "typeinfo"
#include "exception"
#include <stddef.h>
#include "gansidecl.h" /* Needed to support macros used in eh-common.h. */
#include "eh-common.h"

/* Define terminate, unexpected, set_terminate, set_unexpected as
   well as the default terminate func and default unexpected func.  */

extern std::terminate_handler __terminate_func __attribute__((__noreturn__));
using std::terminate;

void
std::terminate ()
{
  __terminate_func ();
}

void
__default_unexpected ()
{
  terminate ();
}

static std::unexpected_handler __unexpected_func __attribute__((__noreturn__))
  = __default_unexpected;

std::terminate_handler
std::set_terminate (std::terminate_handler func)
{
  std::terminate_handler old = __terminate_func;

  __terminate_func = func;
  return old;
}

std::unexpected_handler
std::set_unexpected (std::unexpected_handler func)
{
  std::unexpected_handler old = __unexpected_func;

  __unexpected_func = func;
  return old;
}

void
std::unexpected ()
{
  __unexpected_func ();
}

/* C++-specific state about the current exception.
   This must match init_exception_processing().

   Note that handlers and caught are not redundant; when rethrown, an
   exception can have multiple active handlers and still be considered
   uncaught.  */

struct cp_eh_info
{
  __eh_info eh_info;
  void *value;
  void *type;
  void (*cleanup)(void *, int);
  bool caught;
  cp_eh_info *next;
  long handlers;
  void *original_value;
};

/* Language-specific EH info pointer, defined in libgcc2. */

extern "C" cp_eh_info **__get_eh_info (); 	// actually void **

/* Is P the type_info node for a pointer of some kind?  */

extern bool __is_pointer (void *);


/* OLD Compiler hook to return a pointer to the info for the current exception.
   Used by get_eh_info ().  This fudges the actualy returned value to
   point to the beginning of what USE to be the cp_eh_info structure.
   THis is so that old code that dereferences this pointer will find
   things where it expects it to be.*/
extern "C" void *
__cp_exception_info (void)
{
  return &((*__get_eh_info ())->value);
}

#define CP_EH_INFO ((cp_eh_info *) *__get_eh_info ())

/* Old Compiler hook to return a pointer to the info for the current exception.
   Used by get_eh_info ().  */

extern "C" cp_eh_info *
__cp_eh_info (void)
{
  cp_eh_info *p = CP_EH_INFO;
  return p;
}

/* Compiler hook to return a pointer to the info for the current exception,
   Set the caught bit, and increment the number of handlers that are
   looking at this exception. This makes handlers smaller. */

extern "C" cp_eh_info *
__start_cp_handler (void)
{
  cp_eh_info *p = CP_EH_INFO;
  p->caught = 1;
  p->handlers++;
  return p;
}

/* Allocate a buffer for a cp_eh_info and an exception object of size SIZE,
   and return a pointer to the beginning of the object's space.  */

extern "C" void * malloc (size_t);
extern "C" void *
__eh_alloc (size_t size)
{
  void *p = malloc (size);
  if (p == 0)
    terminate ();
  return p;
}

/* Free the memory for an cp_eh_info and associated exception, given
   a pointer to the cp_eh_info.  */

extern "C" void free (void *);
extern "C" void
__eh_free (void *p)
{
  free (p);
}


typedef void * (* rtimetype) (void);

extern "C" void *
__cplus_type_matcher (cp_eh_info *info, rtimetype match_info, 
                                 exception_descriptor *exception_table)
{
  void *ret;

  /* No exception table implies the old style mechanism, so don't check. */
  if (exception_table != NULL 
      && exception_table->lang.language != EH_LANG_C_plus_plus)
    return NULL;

  if (match_info == CATCH_ALL_TYPE)
    return info->value;

  /* we don't worry about version info yet, there is only one version! */
  
  void *match_type = match_info ();
  ret = __throw_type_match_rtti (match_type, info->type, info->original_value);
  /* change value of exception */
  if (ret)
    info->value = ret;
  return ret;
}


/* Compiler hook to push a new exception onto the stack.
   Used by expand_throw().  */

extern "C" void
__cp_push_exception (void *value, void *type, void (*cleanup)(void *, int))
{
  cp_eh_info *p = (cp_eh_info *) __eh_alloc (sizeof (cp_eh_info));

  p->value = value;
  p->type = type;
  p->cleanup = cleanup;
  p->handlers = 0;
  p->caught = false;
  p->original_value = value;

  p->eh_info.match_function = __cplus_type_matcher;
  p->eh_info.language = EH_LANG_C_plus_plus;
  p->eh_info.version = 1;

  cp_eh_info **q = __get_eh_info ();

  p->next = *q;
  *q = p;
}

/* Compiler hook to pop an exception that has been finalized.  Used by
   push_eh_cleanup().  P is the info for the exception caught by the
   current catch block.  */

extern "C" void
__cp_pop_exception (cp_eh_info *p)
{
  cp_eh_info **q = __get_eh_info ();

  --p->handlers;

  /* Don't really pop if there are still active handlers for our exception,
     or if our exception is being rethrown (i.e. if the active exception is
     our exception and it is uncaught).  */
  if (p->handlers != 0
      || (p == *q && !p->caught))
    return;

  for (; *q; q = &((*q)->next))
    if (*q == p)
      break;

  if (! *q)
    terminate ();

  *q = p->next;

  if (p->cleanup)
    /* 2 is a magic value for destructors; see build_delete().  */
    p->cleanup (p->value, 2);

  if (! __is_pointer (p->type))
    __eh_free (p->original_value);  // value may have been co-erced.

  __eh_free (p);
}

extern "C" void
__uncatch_exception (void)
{
  cp_eh_info *p = CP_EH_INFO;
  if (p == 0)
    terminate ();
  p->caught = false;
}

/* As per [except.unexpected]:
   If an exception is thrown, we check it against the spec.  If it doesn't
   match, we call unexpected ().  If unexpected () throws, we check that
   exception against the spec.  If it doesn't match, if the spec allows
   bad_exception we throw that; otherwise we call terminate ().

   The compiler treats an exception spec as a try block with a generic
   handler that just calls this function with a list of the allowed
   exception types, so we have an active exception that can be rethrown.

   This function does not return.  */   

extern "C" void
__check_eh_spec (int n, const void **spec)
{
  cp_eh_info *p = CP_EH_INFO;

  for (int i = 0; i < n; ++i)
    {
      if (__throw_type_match_rtti (spec[i], p->type, p->value))
	throw;
    }

  try
    {
      std::unexpected ();
    }
  catch (...)
    {
      // __exception_info is an artificial var pushed into each catch block.
      if (p != __exception_info)
	{
	  p = __exception_info;
	  for (int i = 0; i < n; ++i)
	    {
	      if (__throw_type_match_rtti (spec[i], p->type, p->value))
		throw;
	    }
	}

      const std::type_info &bad_exc = typeid (std::bad_exception);
      for (int i = 0; i < n; ++i)
	{
	  if (__throw_type_match_rtti (spec[i], &bad_exc, p->value))
	    throw std::bad_exception ();
	}

      terminate ();
    }
}

extern "C" void
__throw_bad_cast (void)
{
  throw std::bad_cast ();
}

extern "C" void
__throw_bad_typeid (void)
{
  throw std::bad_typeid ();
}

/* Has the current exception been caught?  */

bool
std::uncaught_exception ()
{
  cp_eh_info *p = CP_EH_INFO;
  return p && ! p->caught;
}

const char * std::exception::
what () const
{
  return typeid (*this).name ();
}
