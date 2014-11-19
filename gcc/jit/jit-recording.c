/* Internals of libgccjit: classes for recording calls made to the JIT API.
   Copyright (C) 2013-2014 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "opts.h"
#include "tree.h"
#include "pretty-print.h"

#include <pthread.h>

#include "jit-common.h"
#include "jit-builtins.h"
#include "jit-recording.h"
#include "jit-playback.h"

namespace gcc {
namespace jit {

// class dump

dump::dump (recording::context &ctxt,
	    const char *filename,
	    bool update_locations)
: m_ctxt (ctxt),
  m_filename (filename),
  m_update_locations (update_locations),
  m_line (0),
  m_column (0)
{
  m_file = fopen (filename, "w");
  if (!m_file)
    ctxt.add_error (NULL,
		    "error opening dump file %s for writing: %s",
		    filename,
		    xstrerror (errno));
}

dump::~dump ()
{
  if (m_file)
    {
      int err = fclose (m_file);
      if (err)
	m_ctxt.add_error (NULL,
			  "error closing dump file %s: %s",
			  m_filename,
			  xstrerror (errno));
    }
}

/* Write the given message to the dump, using printf-formatting
   conventions, updating the line/column within the dump.

   Emit an error on the context if a failure occurs.  */

void
dump::write (const char *fmt, ...)
{
  va_list ap;
  char *buf = NULL;

  /* If there was an error opening the file, we've already reported it.
     Don't attempt further work.  */
  if (!m_file)
    return;

  va_start (ap, fmt);
  vasprintf (&buf, fmt, ap);
  va_end (ap);

  if (!buf)
    {
      m_ctxt.add_error (NULL, "malloc failure writing to dumpfile %s",
			m_filename);
      return;
    }

  if (fwrite (buf, strlen (buf), 1, m_file) != 1)
    m_ctxt.add_error (NULL, "error writing to dump file %s",
		      m_filename);

  /* Update line/column: */
  for (const char *ptr = buf; *ptr; ptr++)
    {
      if ('\n' == *ptr)
	{
	  m_line++;
	  m_column = 0;
	}
      else
	m_column++;
    }

  free (buf);
}

/* Construct a gcc::jit::recording::location instance for the current
   location within the dump.  */

recording::location *
dump::make_location () const
{
  return m_ctxt.new_location (m_filename, m_line, m_column);
}

/**********************************************************************
 Recording.
 **********************************************************************/

/* Get the playback::location for the given recording::location,
   handling a NULL input with a NULL output.  */

playback::location *
recording::playback_location (replayer *r, recording::location *loc)
{
  if (loc)
    return loc->playback_location (r);
  else
    return NULL;
}

/* Get a const char * for the given recording::string
   handling a NULL input with a NULL output.  */

const char *
recording::playback_string (recording::string *str)
{
  if (str)
    return str->c_str ();
  else
    return NULL;
}

/* Get the playback::block for the given recording::block,
   handling a NULL input with a NULL output.  */

playback::block *
recording::playback_block (recording::block *b)
{
  if (b)
    return b->playback_block ();
  else
    return NULL;
}

/* Methods of cc::jit::recording::context.  */

/* The constructor for gcc::jit::recording::context, used by
   gcc_jit_context_acquire and gcc_jit_context_new_child_context.  */

recording::context::context (context *parent_ctxt)
  : m_parent_ctxt (parent_ctxt),
    m_error_count (0),
    m_first_error_str (NULL),
    m_owns_first_error_str (false),
    m_mementos (),
    m_compound_types (),
    m_functions (),
    m_FILE_type (NULL),
    m_builtins_manager(NULL)
{
  if (parent_ctxt)
    {
      /* Inherit options from parent.
         Note that the first memcpy means copying pointers to strings.  */
      memcpy (m_str_options,
              parent_ctxt->m_str_options,
              sizeof (m_str_options));
      memcpy (m_int_options,
              parent_ctxt->m_int_options,
              sizeof (m_int_options));
      memcpy (m_bool_options,
              parent_ctxt->m_bool_options,
              sizeof (m_bool_options));
    }
  else
    {
      memset (m_str_options, 0, sizeof (m_str_options));
      memset (m_int_options, 0, sizeof (m_int_options));
      memset (m_bool_options, 0, sizeof (m_bool_options));
    }

  memset (m_basic_types, 0, sizeof (m_basic_types));
}

/* The destructor for gcc::jit::recording::context, implicitly used by
   gcc_jit_context_release.  */

recording::context::~context ()
{
  int i;
  memento *m;
  FOR_EACH_VEC_ELT (m_mementos, i, m)
    {
      delete m;
    }

  if (m_builtins_manager)
    delete m_builtins_manager;

  if (m_owns_first_error_str)
    free (m_first_error_str);
}

/* Add the given mememto to the list of those tracked by this
   gcc::jit::recording::context, so that e.g. it can be deleted
   when this context is released.  */

void
recording::context::record (memento *m)
{
  gcc_assert (m);

  m_mementos.safe_push (m);
}

/* Replay this context (and any parents) into the given replayer.  */

void
recording::context::replay_into (replayer *r)
{
  int i;
  memento *m;

  /* If we have a parent context, we must replay it.  This will
     recursively walk backwards up the historical tree, then replay things
     forwards "in historical order", starting with the ultimate parent
     context, until we reach the "this" context.

     Note that we fully replay the parent, then fully replay the child,
     which means that inter-context references can only exist from child
     to parent, not the other way around.

     All of this replaying is suboptimal - it would be better to do the
     work for the parent context *once*, rather than replaying the parent
     every time we replay each child.  However, fixing this requires deep
     surgery to lifetime-management: we'd need every context family tree
     to have its own GC heap, and to initialize the GCC code to use that
     heap (with a mutex on such a heap).  */
  if (m_parent_ctxt)
    m_parent_ctxt->replay_into (r);

  if (r->errors_occurred ())
    return;

  /* Replay this context's saved operations into r.  */
  FOR_EACH_VEC_ELT (m_mementos, i, m)
    {
      /* Disabled low-level debugging, here if we need it: print what
	 we're replaying.
	 Note that the calls to get_debug_string might lead to more
	 mementos being created for the strings.
	 This can also be used to exercise the debug_string
	 machinery.  */
      if (0)
	printf ("context %p replaying (%p): %s\n",
		(void *)this, (void *)m, m->get_debug_string ());

      m->replay_into (r);

      if (r->errors_occurred ())
	return;
    }
}

/* During a playback, we associate objects from the recording with
   their counterparts during this playback.

   For simplicity, we store this within the recording objects.

   The following method cleans away these associations, to ensure that
   we never have out-of-date associations lingering on subsequent
   playbacks (the objects pointed to are GC-managed, but the
   recording objects don't own refs to them).  */

void
recording::context::disassociate_from_playback ()
{
  int i;
  memento *m;

  if (m_parent_ctxt)
    m_parent_ctxt->disassociate_from_playback ();

  FOR_EACH_VEC_ELT (m_mementos, i, m)
    {
      m->set_playback_obj (NULL);
    }
}

/* Create a recording::string instance and add it to this context's list
   of mementos.

   This creates a fresh copy of the given 0-terminated buffer.  */

recording::string *
recording::context::new_string (const char *text)
{
  if (!text)
    return NULL;

  recording::string *result = new string (this, text);
  record (result);
  return result;
}

/* Create a recording::location instance and add it to this context's
   list of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_location.  */

recording::location *
recording::context::new_location (const char *filename,
				 int line,
				 int column)
{
  recording::location *result =
    new recording::location (this,
			    new_string (filename),
			    line, column);
  record (result);
  return result;
}

/* If we haven't seen this enum value yet, create a recording::type
   instance and add it to this context's list of mementos.

   If we have seen it before, reuse our cached value, so that repeated
   calls on the context give the same object.

   If we have a parent context, the cache is within the ultimate
   ancestor context.

   Implements the post-error-checking part of
   gcc_jit_context_get_type.  */

recording::type *
recording::context::get_type (enum gcc_jit_types kind)
{
  if (!m_basic_types[kind])
    {
      if (m_parent_ctxt)
	m_basic_types[kind] = m_parent_ctxt->get_type (kind);
      else
	{
	  recording::type *result = new memento_of_get_type (this, kind);
	  record (result);
	  m_basic_types[kind] = result;
	}
    }

  return m_basic_types[kind];
}

/* Get a recording::type instance for the given size and signedness.
   This is implemented in terms of recording::context::get_type
   above.

   Implements the post-error-checking part of
   gcc_jit_context_get_int_type.  */

recording::type *
recording::context::get_int_type (int num_bytes, int is_signed)
{
  /* We can't use a switch here since some of the values are macros affected
     by options; e.g. i386.h has
       #define LONG_TYPE_SIZE (TARGET_X32 ? 32 : BITS_PER_WORD)
     Compare with tree.c's make_or_reuse_type.  Note that the _SIZE macros
     are in bits, rather than bytes.
  */
  const int num_bits = num_bytes * 8;
  if (num_bits == INT_TYPE_SIZE)
    return get_type (is_signed
		     ? GCC_JIT_TYPE_INT
		     : GCC_JIT_TYPE_UNSIGNED_INT);
  if (num_bits == CHAR_TYPE_SIZE)
    return get_type (is_signed
		     ? GCC_JIT_TYPE_SIGNED_CHAR
		     : GCC_JIT_TYPE_UNSIGNED_CHAR);
  if (num_bits == SHORT_TYPE_SIZE)
    return get_type (is_signed
		     ? GCC_JIT_TYPE_SHORT
		     : GCC_JIT_TYPE_UNSIGNED_SHORT);
  if (num_bits == LONG_TYPE_SIZE)
    return get_type (is_signed
		     ? GCC_JIT_TYPE_LONG
		     : GCC_JIT_TYPE_UNSIGNED_LONG);
  if (num_bits == LONG_LONG_TYPE_SIZE)
    return get_type (is_signed
		     ? GCC_JIT_TYPE_LONG_LONG
		     : GCC_JIT_TYPE_UNSIGNED_LONG_LONG);

  /* Some other size, not corresponding to the C int types.  */
  /* To be written: support arbitrary other sizes, sharing by
     memoizing at the recording::context level?  */
  gcc_unreachable ();
}

/* Create a recording::type instance and add it to this context's list
   of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_array_type.  */

recording::type *
recording::context::new_array_type (recording::location *loc,
				    recording::type *element_type,
				    int num_elements)
{
  if (struct_ *s = element_type->dyn_cast_struct ())
    if (!s->get_fields ())
      {
	add_error (NULL,
		   "cannot create an array of type %s"
		   " until the fields have been set",
		   s->get_name ()->c_str ());
	return NULL;
      }
  recording::type *result =
    new recording::array_type (this, loc, element_type, num_elements);
  record (result);
  return result;
}

/* Create a recording::field instance and add it to this context's list
   of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_field.  */

recording::field *
recording::context::new_field (recording::location *loc,
			       recording::type *type,
			       const char *name)
{
  recording::field *result =
    new recording::field (this, loc, type, new_string (name));
  record (result);
  return result;
}

/* Create a recording::struct_ instance and add it to this context's
   list of mementos and list of compound types.

   Implements the post-error-checking part of
   gcc_jit_context_new_struct_type.  */

recording::struct_ *
recording::context::new_struct_type (recording::location *loc,
				     const char *name)
{
  recording::struct_ *result = new struct_ (this, loc, new_string (name));
  record (result);
  m_compound_types.safe_push (result);
  return result;
}

/* Create a recording::union_ instance and add it to this context's
   list of mementos and list of compound types.

   Implements the first post-error-checking part of
   gcc_jit_context_new_union_type.  */

recording::union_ *
recording::context::new_union_type (recording::location *loc,
				    const char *name)
{
  recording::union_ *result = new union_ (this, loc, new_string (name));
  record (result);
  m_compound_types.safe_push (result);
  return result;
}

/* Create a recording::type instance and add it to this context's list
   of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_function_ptr_type.  */

recording::type *
recording::context::new_function_ptr_type (recording::location *, /* unused loc */
					   recording::type *return_type,
					   int num_params,
					   recording::type **param_types,
					   int is_variadic)
{
  recording::function_type *fn_type =
    new function_type (this,
		       return_type,
		       num_params,
		       param_types,
		       is_variadic);
  record (fn_type);

  /* Return a pointer-type to the the function type.  */
  return fn_type->get_pointer ();
}

/* Create a recording::param instance and add it to this context's list
   of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_param.  */

recording::param *
recording::context::new_param (recording::location *loc,
			       recording::type *type,
			       const char *name)
{
  recording::param *result = new recording::param (this, loc, type, new_string (name));
  record (result);
  return result;
}

/* Create a recording::function instance and add it to this context's list
   of mementos and list of functions.

   Implements the post-error-checking part of
   gcc_jit_context_new_function.  */

recording::function *
recording::context::new_function (recording::location *loc,
				  enum gcc_jit_function_kind kind,
				  recording::type *return_type,
				  const char *name,
				  int num_params,
				  recording::param **params,
				  int is_variadic,
				  enum built_in_function builtin_id)
{
  recording::function *result =
    new recording::function (this,
			     loc, kind, return_type,
			     new_string (name),
			     num_params, params, is_variadic,
			     builtin_id);
  record (result);
  m_functions.safe_push (result);

  return result;
}

/* Get a recording::function instance, which is lazily-created and added
   to the context's lists of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_get_builtin_function.  */

recording::function *
recording::context::get_builtin_function (const char *name)
{
  if (!m_builtins_manager)
    m_builtins_manager = new builtins_manager (this);
  return m_builtins_manager->get_builtin_function (name);
}

/* Create a recording::global instance and add it to this context's list
   of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_global.  */

recording::lvalue *
recording::context::new_global (recording::location *loc,
				recording::type *type,
				const char *name)
{
  recording::lvalue *result =
    new recording::global (this, loc, type, new_string (name));
  record (result);
  return result;
}

/* Create a recording::memento_of_new_rvalue_from_int instance and add
   it to this context's list of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_rvalue_from_int.  */

recording::rvalue *
recording::context::new_rvalue_from_int (recording::type *type,
					 int value)
{
  recording::rvalue *result =
    new memento_of_new_rvalue_from_int (this, NULL, type, value);
  record (result);
  return result;
}

/* Create a recording::memento_of_new_rvalue_from_double instance and
   add it to this context's list of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_rvalue_from_double.  */

recording::rvalue *
recording::context::new_rvalue_from_double (recording::type *type,
					    double value)
{
  recording::rvalue *result =
    new memento_of_new_rvalue_from_double (this, NULL, type, value);
  record (result);
  return result;
}

/* Create a recording::memento_of_new_rvalue_from_ptr instance and add
   it to this context's list of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_rvalue_from_ptr.  */

recording::rvalue *
recording::context::new_rvalue_from_ptr (recording::type *type,
					 void *value)
{
  recording::rvalue *result =
    new memento_of_new_rvalue_from_ptr (this, NULL, type, value);
  record (result);
  return result;
}

/* Create a recording::memento_of_new_string_literal instance and add it
   to this context's list of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_string_literal.  */

recording::rvalue *
recording::context::new_string_literal (const char *value)
{
  recording::rvalue *result =
    new memento_of_new_string_literal (this, NULL, new_string (value));
  record (result);
  return result;
}

/* Create a recording::unary_op instance and add it to this context's
   list of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_unary_op.  */

recording::rvalue *
recording::context::new_unary_op (recording::location *loc,
				  enum gcc_jit_unary_op op,
				  recording::type *result_type,
				  recording::rvalue *a)
{
  recording::rvalue *result =
    new unary_op (this, loc, op, result_type, a);
  record (result);
  return result;
}

/* Create a recording::binary_op instance and add it to this context's
   list of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_binary_op.  */

recording::rvalue *
recording::context::new_binary_op (recording::location *loc,
				   enum gcc_jit_binary_op op,
				   recording::type *result_type,
				   recording::rvalue *a,
				   recording::rvalue *b)
{
  recording::rvalue *result =
    new binary_op (this, loc, op, result_type, a, b);
  record (result);
  return result;
}

/* Create a recording::comparison instance and add it to this context's
   list of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_comparison.  */

recording::rvalue *
recording::context::new_comparison (recording::location *loc,
				    enum gcc_jit_comparison op,
				    recording::rvalue *a,
				    recording::rvalue *b)
{
  recording::rvalue *result = new comparison (this, loc, op, a, b);
  record (result);
  return result;
}

/* Create a recording::cast instance and add it to this context's list
   of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_cast.  */

recording::rvalue *
recording::context::new_cast (recording::location *loc,
			      recording::rvalue *expr,
			      recording::type *type_)
{
  recording::rvalue *result = new cast (this, loc, expr, type_);
  record (result);
  return result;
}

/* Create a recording::call instance and add it to this context's list
   of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_call.  */

recording::rvalue *
recording::context::new_call (recording::location *loc,
			      function *func,
			      int numargs , recording::rvalue **args)
{
  recording::rvalue *result = new call (this, loc, func, numargs, args);
  record (result);
  return result;
}

/* Create a recording::call_through_ptr instance and add it to this
   context's list of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_call_through_ptr.  */

recording::rvalue *
recording::context::new_call_through_ptr (recording::location *loc,
					  recording::rvalue *fn_ptr,
					  int numargs,
					  recording::rvalue **args)
  {
  recording::rvalue *result = new call_through_ptr (this, loc, fn_ptr, numargs, args);
  record (result);
  return result;
}

/* Create a recording::array_access instance and add it to this context's list
   of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_array_access.  */

recording::lvalue *
recording::context::new_array_access (recording::location *loc,
				      recording::rvalue *ptr,
				      recording::rvalue *index)
{
  recording::lvalue *result = new array_access (this, loc, ptr, index);
  record (result);
  return result;
}

/* Set the given string option for this context, or add an error if
   it's not recognized.

   Implements the post-error-checking part of
   gcc_jit_context_set_str_option.  */

void
recording::context::set_str_option (enum gcc_jit_str_option opt,
				    const char *value)
{
  if (opt < 0 || opt >= GCC_JIT_NUM_STR_OPTIONS)
    {
      add_error (NULL,
		 "unrecognized (enum gcc_jit_str_option) value: %i", opt);
      return;
    }
  m_str_options[opt] = value;
}

/* Set the given integer option for this context, or add an error if
   it's not recognized.

   Implements the post-error-checking part of
   gcc_jit_context_set_int_option.  */

void
recording::context::set_int_option (enum gcc_jit_int_option opt,
				    int value)
{
  if (opt < 0 || opt >= GCC_JIT_NUM_INT_OPTIONS)
    {
      add_error (NULL,
		 "unrecognized (enum gcc_jit_int_option) value: %i", opt);
      return;
    }
  m_int_options[opt] = value;
}

/* Set the given boolean option for this context, or add an error if
   it's not recognized.

   Implements the post-error-checking part of
   gcc_jit_context_set_bool_option.  */

void
recording::context::set_bool_option (enum gcc_jit_bool_option opt,
				     int value)
{
  if (opt < 0 || opt >= GCC_JIT_NUM_BOOL_OPTIONS)
    {
      add_error (NULL,
		 "unrecognized (enum gcc_jit_bool_option) value: %i", opt);
      return;
    }
  m_bool_options[opt] = value ? true : false;
}

/* This mutex guards gcc::jit::recording::context::compile, so that only
   one thread can be accessing the bulk of GCC's state at once.  */

static pthread_mutex_t jit_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Validate this context, and if it passes, compile it within a
   mutex.

   Implements the post-error-checking part of
   gcc_jit_context_compile.  */

result *
recording::context::compile ()
{
  validate ();

  if (errors_occurred ())
    return NULL;

  /* Acquire the big GCC mutex. */
  pthread_mutex_lock (&jit_mutex);
  gcc_assert (NULL == ::gcc::jit::active_playback_ctxt);

  /* Set up a playback context.  */
  ::gcc::jit::playback::context replayer (this);
  ::gcc::jit::active_playback_ctxt = &replayer;

  result *result_obj = replayer.compile ();

  /* Release the big GCC mutex. */
  ::gcc::jit::active_playback_ctxt = NULL;
  pthread_mutex_unlock (&jit_mutex);

  return result_obj;
}

/* Format the given error using printf's conventions, print
   it to stderr, and add it to the context.  */

void
recording::context::add_error (location *loc, const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  add_error_va (loc, fmt, ap);
  va_end (ap);
}

/* Format the given error using printf's conventions, print
   it to stderr, and add it to the context.  */

void
recording::context::add_error_va (location *loc, const char *fmt, va_list ap)
{
  char *malloced_msg;
  const char *errmsg;
  bool has_ownership;

  vasprintf (&malloced_msg, fmt, ap);
  if (malloced_msg)
    {
      errmsg = malloced_msg;
      has_ownership = true;
    }
  else
    {
      errmsg = "out of memory generating error message";
      has_ownership = false;
    }

  const char *ctxt_progname =
    get_str_option (GCC_JIT_STR_OPTION_PROGNAME);
  if (!ctxt_progname)
    ctxt_progname = "libgccjit.so";

  if (loc)
    fprintf (stderr, "%s: %s: error: %s\n",
	     ctxt_progname,
	     loc->get_debug_string (),
	     errmsg);
  else
    fprintf (stderr, "%s: error: %s\n",
	     ctxt_progname,
	     errmsg);

  if (!m_error_count)
    {
      m_first_error_str = const_cast <char *> (errmsg);
      m_owns_first_error_str = has_ownership;
    }
  else
    if (has_ownership)
      free (malloced_msg);

  m_error_count++;
}

/* Get the message for the first error that occurred on this context, or
   NULL if no errors have occurred on it.

   Implements the post-error-checking part of
   gcc_jit_context_get_first_error.  */

const char *
recording::context::get_first_error () const
{
  return m_first_error_str;
}

/* Lazily generate and record a recording::type representing an opaque
   struct named "FILE".

   For use if client code tries to dereference the result of
   get_type (GCC_JIT_TYPE_FILE_PTR).  */

recording::type *
recording::context::get_opaque_FILE_type ()
{
  if (!m_FILE_type)
    m_FILE_type = new_struct_type (NULL, "FILE");
  return m_FILE_type;
}

/* Dump a C-like representation of the given context to the given path.
   If UPDATE_LOCATIONS is true, update the locations within the
   context's mementos to point to the dumpfile.

   Implements the post-error-checking part of
   gcc_jit_context_dump_to_file.  */

void
recording::context::dump_to_file (const char *path, bool update_locations)
{
  int i;
  dump d (*this, path, update_locations);

  /* Forward declaration of structs and unions.  */
  compound_type *st;
  FOR_EACH_VEC_ELT (m_compound_types, i, st)
    {
      d.write ("%s;\n\n", st->get_debug_string ());
    }

  /* Content of structs, where set.  */
  FOR_EACH_VEC_ELT (m_compound_types, i, st)
    if (st->get_fields ())
      {
	st->get_fields ()->write_to_dump (d);
	d.write ("\n");
      }

  function *fn;
  FOR_EACH_VEC_ELT (m_functions, i, fn)
    {
      fn->write_to_dump (d);
    }
}

/* This is a pre-compilation check for the context (and any parents).

   Detect errors within the context, adding errors if any are found.  */

void
recording::context::validate ()
{
  if (m_parent_ctxt)
    m_parent_ctxt->validate ();

  int i;
  function *fn;
  FOR_EACH_VEC_ELT (m_functions, i, fn)
    fn->validate ();
}

/* The implementation of class gcc::jit::recording::memento.  */

/* Get a (const char *) debug description of the given memento, by
   calling the pure-virtual make_debug_string hook, caching the
   result.

   It is intended that this should only be called in debugging and
   error-handling paths, so this doesn't need to be particularly
   optimized.  */

const char *
recording::memento::get_debug_string ()
{
  if (!m_debug_string)
    m_debug_string = make_debug_string ();
  return m_debug_string->c_str ();
}

/* Default implementation of recording::memento::write_to_dump, writing
   an indented form of the memento's debug string to the dump.  */

void
recording::memento::write_to_dump (dump &d)
{
  d.write("  %s\n", get_debug_string ());
}

/* The implementation of class gcc::jit::recording::string.  */

/* Constructor for gcc::jit::recording::string::string, allocating a
   copy of the given text using new char[].  */

recording::string::string (context *ctxt, const char *text)
  : memento (ctxt)
{
  m_len = strlen (text);
  m_buffer = new char[m_len + 1];
  strcpy (m_buffer, text);
}

/* Destructor for gcc::jit::recording::string::string.  */

recording::string::~string ()
{
  delete[] m_buffer;
}

/* Function for making gcc::jit::recording::string instances on a
   context via printf-style formatting.

   It is intended that this should only be called in debugging and
   error-handling paths, so this doesn't need to be particularly
   optimized, hence the double-copy of the string is acceptable.  */

recording::string *
recording::string::from_printf (context *ctxt, const char *fmt, ...)
{
  va_list ap;
  char *buf = NULL;
  recording::string *result;

  va_start (ap, fmt);
  vasprintf (&buf, fmt, ap);
  va_end (ap);

  if (!buf)
    {
      ctxt->add_error (NULL, "malloc failure");
      return NULL;
    }

  result = ctxt->new_string (buf);
  free (buf);
  return result;
}

/* Implementation of recording::memento::make_debug_string for strings,
   wrapping the given string in quotes and escaping as necessary.  */

recording::string *
recording::string::make_debug_string ()
{
  /* Hack to avoid infinite recursion into strings when logging all
     mementos: don't re-escape strings:  */
  if (m_buffer[0] == '"')
    return this;

  /* Wrap in quotes and do escaping etc */

  size_t sz = (1 /* opening quote */
	       + (m_len * 2) /* each char might get escaped */
	       + 1 /* closing quote */
	       + 1); /* nil termintator */
  char *tmp = new char[sz];
  size_t len = 0;

#define APPEND(CH)  do { gcc_assert (len < sz); tmp[len++] = (CH); } while (0)
  APPEND('"'); /* opening quote */
  for (size_t i = 0; i < m_len ; i++)
    {
      char ch = m_buffer[i];
      if (ch == '\t' || ch == '\n' || ch == '\\' || ch == '"')
	APPEND('\\');
      APPEND(ch);
    }
  APPEND('"'); /* closing quote */
#undef APPEND
  tmp[len] = '\0'; /* nil termintator */

  string *result = m_ctxt->new_string (tmp);

  delete[] tmp;
  return result;
}

/* The implementation of class gcc::jit::recording::location.  */

/* Implementation of recording::memento::replay_into for locations.

   Create a new playback::location and store it into the
   recording::location's m_playback_obj field.  */

void
recording::location::replay_into (replayer *r)
{
  m_playback_obj = r->new_location (this,
				    m_filename->c_str (),
				    m_line,
				    m_column);
}

/* Implementation of recording::memento::make_debug_string for locations,
   turning them into the usual form:
     FILENAME:LINE:COLUMN
   like we do when emitting diagnostics.  */

recording::string *
recording::location::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "%s:%i:%i",
			      m_filename->c_str (), m_line, m_column);
}

/* The implementation of class gcc::jit::recording::type.  */

/* Given a type T, get the type T*.

   If this doesn't already exist, generate a new memento_of_get_pointer
   instance and add it to this type's context's list of mementos.

   Otherwise, use the cached type.

   Implements the post-error-checking part of
   gcc_jit_type_get_pointer.  */

recording::type *
recording::type::get_pointer ()
{
  if (!m_pointer_to_this_type)
    {
      m_pointer_to_this_type = new memento_of_get_pointer (this);
      m_ctxt->record (m_pointer_to_this_type);
    }
  return m_pointer_to_this_type;
}

/* Given a type T, get the type const T.

   Implements the post-error-checking part of
   gcc_jit_type_get_const.  */

recording::type *
recording::type::get_const ()
{
  recording::type *result = new memento_of_get_const (this);
  m_ctxt->record (result);
  return result;
}

/* Given a type T, get the type volatile T.

   Implements the post-error-checking part of
   gcc_jit_type_get_volatile.  */

recording::type *
recording::type::get_volatile ()
{
  recording::type *result = new memento_of_get_volatile (this);
  m_ctxt->record (result);
  return result;
}

/* Implementation of pure virtual hook recording::type::dereference for
   recording::memento_of_get_type.  */

recording::type *
recording::memento_of_get_type::dereference ()
{
  switch (m_kind)
    {
    default: gcc_unreachable ();

    case GCC_JIT_TYPE_VOID:
      return NULL;

    case GCC_JIT_TYPE_VOID_PTR:
      return m_ctxt->get_type (GCC_JIT_TYPE_VOID);

    case GCC_JIT_TYPE_BOOL:
    case GCC_JIT_TYPE_CHAR:
    case GCC_JIT_TYPE_SIGNED_CHAR:
    case GCC_JIT_TYPE_UNSIGNED_CHAR:
    case GCC_JIT_TYPE_SHORT:
    case GCC_JIT_TYPE_UNSIGNED_SHORT:
    case GCC_JIT_TYPE_INT:
    case GCC_JIT_TYPE_UNSIGNED_INT:
    case GCC_JIT_TYPE_LONG:
    case GCC_JIT_TYPE_UNSIGNED_LONG:
    case GCC_JIT_TYPE_LONG_LONG:
    case GCC_JIT_TYPE_UNSIGNED_LONG_LONG:
    case GCC_JIT_TYPE_FLOAT:
    case GCC_JIT_TYPE_DOUBLE:
    case GCC_JIT_TYPE_LONG_DOUBLE:
      /* Not a pointer: */
      return NULL;

    case GCC_JIT_TYPE_CONST_CHAR_PTR:
      return m_ctxt->get_type (GCC_JIT_TYPE_CHAR)->get_const ();

    case GCC_JIT_TYPE_SIZE_T:
      /* Not a pointer: */
      return NULL;

    case GCC_JIT_TYPE_FILE_PTR:
      /* Give the client code back an opaque "struct FILE".  */
      return m_ctxt->get_opaque_FILE_type ();
    }
}

/* Implementation of pure virtual hook recording::type::is_int for
   recording::memento_of_get_type.  */

bool
recording::memento_of_get_type::is_int () const
{
  switch (m_kind)
    {
    default: gcc_unreachable ();

    case GCC_JIT_TYPE_VOID:
      return false;

    case GCC_JIT_TYPE_VOID_PTR:
      return false;

    case GCC_JIT_TYPE_BOOL:
      return false;

    case GCC_JIT_TYPE_CHAR:
    case GCC_JIT_TYPE_SIGNED_CHAR:
    case GCC_JIT_TYPE_UNSIGNED_CHAR:
    case GCC_JIT_TYPE_SHORT:
    case GCC_JIT_TYPE_UNSIGNED_SHORT:
    case GCC_JIT_TYPE_INT:
    case GCC_JIT_TYPE_UNSIGNED_INT:
    case GCC_JIT_TYPE_LONG:
    case GCC_JIT_TYPE_UNSIGNED_LONG:
    case GCC_JIT_TYPE_LONG_LONG:
    case GCC_JIT_TYPE_UNSIGNED_LONG_LONG:
      return true;

    case GCC_JIT_TYPE_FLOAT:
    case GCC_JIT_TYPE_DOUBLE:
    case GCC_JIT_TYPE_LONG_DOUBLE:
      return false;

    case GCC_JIT_TYPE_CONST_CHAR_PTR:
      return false;

    case GCC_JIT_TYPE_SIZE_T:
      return true;

    case GCC_JIT_TYPE_FILE_PTR:
      return false;
    }
}

/* Implementation of pure virtual hook recording::type::is_float for
   recording::memento_of_get_type.  */

bool
recording::memento_of_get_type::is_float () const
{
  switch (m_kind)
    {
    default: gcc_unreachable ();

    case GCC_JIT_TYPE_VOID:
      return false;

    case GCC_JIT_TYPE_VOID_PTR:
      return false;

    case GCC_JIT_TYPE_BOOL:
      return false;

    case GCC_JIT_TYPE_CHAR:
    case GCC_JIT_TYPE_SIGNED_CHAR:
    case GCC_JIT_TYPE_UNSIGNED_CHAR:
    case GCC_JIT_TYPE_SHORT:
    case GCC_JIT_TYPE_UNSIGNED_SHORT:
    case GCC_JIT_TYPE_INT:
    case GCC_JIT_TYPE_UNSIGNED_INT:
    case GCC_JIT_TYPE_LONG:
    case GCC_JIT_TYPE_UNSIGNED_LONG:
    case GCC_JIT_TYPE_LONG_LONG:
    case GCC_JIT_TYPE_UNSIGNED_LONG_LONG:
      return false;

    case GCC_JIT_TYPE_FLOAT:
    case GCC_JIT_TYPE_DOUBLE:
    case GCC_JIT_TYPE_LONG_DOUBLE:
      return true;

    case GCC_JIT_TYPE_CONST_CHAR_PTR:
      return false;

    case GCC_JIT_TYPE_SIZE_T:
      return false;

    case GCC_JIT_TYPE_FILE_PTR:
      return false;
    }
}

/* Implementation of pure virtual hook recording::type::is_bool for
   recording::memento_of_get_type.  */

bool
recording::memento_of_get_type::is_bool () const
{
  switch (m_kind)
    {
    default: gcc_unreachable ();

    case GCC_JIT_TYPE_VOID:
      return false;

    case GCC_JIT_TYPE_VOID_PTR:
      return false;

    case GCC_JIT_TYPE_BOOL:
      return true;

    case GCC_JIT_TYPE_CHAR:
    case GCC_JIT_TYPE_SIGNED_CHAR:
    case GCC_JIT_TYPE_UNSIGNED_CHAR:
    case GCC_JIT_TYPE_SHORT:
    case GCC_JIT_TYPE_UNSIGNED_SHORT:
    case GCC_JIT_TYPE_INT:
    case GCC_JIT_TYPE_UNSIGNED_INT:
    case GCC_JIT_TYPE_LONG:
    case GCC_JIT_TYPE_UNSIGNED_LONG:
    case GCC_JIT_TYPE_LONG_LONG:
    case GCC_JIT_TYPE_UNSIGNED_LONG_LONG:
      return false;

    case GCC_JIT_TYPE_FLOAT:
    case GCC_JIT_TYPE_DOUBLE:
    case GCC_JIT_TYPE_LONG_DOUBLE:
      return false;

    case GCC_JIT_TYPE_CONST_CHAR_PTR:
      return false;

    case GCC_JIT_TYPE_SIZE_T:
      return false;

    case GCC_JIT_TYPE_FILE_PTR:
      return false;
    }
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::memento_of_get_type.  */

void
recording::memento_of_get_type::replay_into (replayer *r)
{
  set_playback_obj (r->get_type (m_kind));
}

/* The implementation of class gcc::jit::recording::memento_of_get_type.  */

/* Descriptive strings for each of enum gcc_jit_types.  */

static const char * const get_type_strings[] = {
  "void",    /* GCC_JIT_TYPE_VOID */
  "void *",  /* GCC_JIT_TYPE_VOID_PTR */

  "bool",  /* GCC_JIT_TYPE_BOOL */

  "char",           /* GCC_JIT_TYPE_CHAR */
  "signed char",    /* GCC_JIT_TYPE_SIGNED_CHAR */
  "unsigned char",  /* GCC_JIT_TYPE_UNSIGNED_CHAR */

  "short",           /* GCC_JIT_TYPE_SHORT */
  "unsigned short",  /* GCC_JIT_TYPE_UNSIGNED_SHORT */

  "int",           /* GCC_JIT_TYPE_INT */
  "unsigned int",  /* GCC_JIT_TYPE_UNSIGNED_INT */

  "long",           /* GCC_JIT_TYPE_LONG  */
  "unsigned long",  /* GCC_JIT_TYPE_UNSIGNED_LONG, */

  "long long",           /* GCC_JIT_TYPE_LONG_LONG */
  "unsigned long long",  /* GCC_JIT_TYPE_UNSIGNED_LONG_LONG */

  "float",        /* GCC_JIT_TYPE_FLOAT */
  "double",       /* GCC_JIT_TYPE_DOUBLE */
  "long double",  /* GCC_JIT_TYPE_LONG_DOUBLE */

  "const char *",  /* GCC_JIT_TYPE_CONST_CHAR_PTR */

  "size_t",  /* GCC_JIT_TYPE_SIZE_T */

  "FILE *"  /* GCC_JIT_TYPE_FILE_PTR */

};

/* Implementation of recording::memento::make_debug_string for
   results of get_type, using a simple table of type names.  */

recording::string *
recording::memento_of_get_type::make_debug_string ()
{
  return m_ctxt->new_string (get_type_strings[m_kind]);
}

/* The implementation of class gcc::jit::recording::memento_of_get_pointer.  */

/* Override of default implementation of
   recording::type::accepts_writes_from for get_pointer.

   Require a pointer type, and allowing writes to
   (const T *) from a (T*), but not the other way around.  */

bool
recording::memento_of_get_pointer::accepts_writes_from (type *rtype)
{
  /* Must be a pointer type: */
  type *rtype_points_to = rtype->is_pointer ();
  if (!rtype_points_to)
    return false;

  /* It's OK to assign to a (const T *) from a (T *).  */
  return m_other_type->unqualified ()
    ->accepts_writes_from (rtype_points_to);
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::memento_of_get_pointer.  */

void
recording::memento_of_get_pointer::replay_into (replayer *)
{
  set_playback_obj (m_other_type->playback_type ()->get_pointer ());
}

/* Implementation of recording::memento::make_debug_string for
   results of get_pointer, adding " *" to the underlying type,
   with special-casing to handle function pointer types.  */

recording::string *
recording::memento_of_get_pointer::make_debug_string ()
{
  /* Special-case function pointer types, to put the "*" in parens between
     the return type and the params (for one level of dereferencing, at
     least).  */
  if (function_type *fn_type = m_other_type->dyn_cast_function_type ())
    return fn_type->make_debug_string_with_ptr ();

  return string::from_printf (m_ctxt,
			      "%s *", m_other_type->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::memento_of_get_const.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::memento_of_get_const.  */

void
recording::memento_of_get_const::replay_into (replayer *)
{
  set_playback_obj (m_other_type->playback_type ()->get_const ());
}

/* Implementation of recording::memento::make_debug_string for
   results of get_const, prepending "const ".  */

recording::string *
recording::memento_of_get_const::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "const %s", m_other_type->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::memento_of_get_volatile.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::memento_of_get_volatile.  */

void
recording::memento_of_get_volatile::replay_into (replayer *)
{
  set_playback_obj (m_other_type->playback_type ()->get_volatile ());
}

/* Implementation of recording::memento::make_debug_string for
   results of get_volatile, prepending "volatile ".  */

recording::string *
recording::memento_of_get_volatile::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "volatile %s", m_other_type->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::array_type */

/* Implementation of pure virtual hook recording::type::dereference for
   recording::array_type.  */

recording::type *
recording::array_type::dereference ()
{
  return m_element_type;
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::array_type.  */

void
recording::array_type::replay_into (replayer *r)
{
  set_playback_obj (r->new_array_type (playback_location (r, m_loc),
				       m_element_type->playback_type (),
				       m_num_elements));
}

/* Implementation of recording::memento::make_debug_string for
   results of new_array_type.  */

recording::string *
recording::array_type::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "%s[%d]",
			      m_element_type->get_debug_string (),
			      m_num_elements);
}

/* The implementation of class gcc::jit::recording::function_type */

/* Constructor for gcc::jit::recording::function_type.  */

recording::function_type::function_type (context *ctxt,
					 type *return_type,
					 int num_params,
					 type **param_types,
					 int is_variadic)
: type (ctxt),
  m_return_type (return_type),
  m_param_types (),
  m_is_variadic (is_variadic)
{
  for (int i = 0; i< num_params; i++)
    m_param_types.safe_push (param_types[i]);
}

/* Implementation of pure virtual hook recording::type::dereference for
   recording::function_type.  */

recording::type *
recording::function_type::dereference ()
{
  return NULL;
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::function_type.  */

void
recording::function_type::replay_into (replayer *r)
{
  /* Convert m_param_types to a vec of playback type.  */
  vec <playback::type *> param_types;
  int i;
  recording::type *type;
  param_types.create (m_param_types.length ());
  FOR_EACH_VEC_ELT (m_param_types, i, type)
    param_types.safe_push (type->playback_type ());

  set_playback_obj (r->new_function_type (m_return_type->playback_type (),
					  &param_types,
					  m_is_variadic));
}

/* Special-casing for make_debug_string for get_pointer results for
   handling (one level) of pointers to functions.  */

recording::string *
recording::function_type::make_debug_string_with_ptr ()
{
  return make_debug_string_with ("(*) ");
}

/* Implementation of recording::memento::make_debug_string for
   results of new_function_type.  */

recording::string *
recording::function_type::make_debug_string ()
{
  return make_debug_string_with ("");
}

/* Build a debug string representation of the form:

     RESULT_TYPE INSERT (PARAM_TYPES)

   for use when handling 0 and 1 level of indirection to this
   function type.  */

recording::string *
recording::function_type::make_debug_string_with (const char *insert)
{
  /* First, build a buffer for the arguments.  */
  /* Calculate length of said buffer.  */
  size_t sz = 1; /* nil terminator */
  for (unsigned i = 0; i< m_param_types.length (); i++)
    {
      sz += strlen (m_param_types[i]->get_debug_string ());
      sz += 2; /* ", " separator */
    }
  if (m_is_variadic)
    sz += 5; /* ", ..." separator and ellipsis */

  /* Now allocate and populate the buffer.  */
  char *argbuf = new char[sz];
  size_t len = 0;

  for (unsigned i = 0; i< m_param_types.length (); i++)
    {
      strcpy (argbuf + len, m_param_types[i]->get_debug_string ());
      len += strlen (m_param_types[i]->get_debug_string ());
      if (i + 1 < m_param_types.length ())
	{
	  strcpy (argbuf + len, ", ");
	  len += 2;
	}
    }
  if (m_is_variadic)
    {
      if (m_param_types.length ())
	{
	  strcpy (argbuf + len, ", ");
	  len += 2;
	}
      strcpy (argbuf + len, "...");
      len += 3;
    }
  argbuf[len] = '\0';

  /* ...and use it to get the string for the call as a whole.  */
  string *result = string::from_printf (m_ctxt,
					"%s %s(%s)",
					m_return_type->get_debug_string (),
					insert,
					argbuf);

  delete[] argbuf;

  return result;
}

/* The implementation of class gcc::jit::recording::field.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::field.  */

void
recording::field::replay_into (replayer *r)
{
  set_playback_obj (r->new_field (playback_location (r, m_loc),
				  m_type->playback_type (),
				  playback_string (m_name)));
}

/* Override the default implementation of
   recording::memento::write_to_dump.  Dump each field
   by dumping a line of the form:
      TYPE NAME;
   so that we can build up a struct/union field-byfield.  */

void
recording::field::write_to_dump (dump &d)
{
  d.write ("  %s %s;\n",
	   m_type->get_debug_string (),
	   m_name->c_str ());
}

/* Implementation of recording::memento::make_debug_string for
   results of new_field.  */

recording::string *
recording::field::make_debug_string ()
{
  return m_name;
}

/* The implementation of class gcc::jit::recording::compound_type */

/* The constructor for gcc::jit::recording::compound_type.  */

recording::compound_type::compound_type (context *ctxt,
					 location *loc,
					 string *name)
: type (ctxt),
  m_loc (loc),
  m_name (name),
  m_fields (NULL)
{
}

/* Set the fields of a compound type.

   Implements the post-error-checking part of
   gcc_jit_struct_set_fields, and is also used by
   gcc_jit_context_new_union_type.  */

void
recording::compound_type::set_fields (location *loc,
				      int num_fields,
				      field **field_array)
{
  m_loc = loc;
  gcc_assert (NULL == m_fields);

  m_fields = new fields (this, num_fields, field_array);
  m_ctxt->record (m_fields);
}

/* Implementation of pure virtual hook recording::type::dereference for
   recording::compound_type.  */

recording::type *
recording::compound_type::dereference ()
{
  return NULL; /* not a pointer */
}

/* The implementation of class gcc::jit::recording::struct_.  */

/* The constructor for gcc::jit::recording::struct_.  */

recording::struct_::struct_ (context *ctxt,
			     location *loc,
			     string *name)
: compound_type (ctxt, loc, name)
{
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::struct_.  */

void
recording::struct_::replay_into (replayer *r)
{
  set_playback_obj (
    r->new_compound_type (playback_location (r, get_loc ()),
			  get_name ()->c_str (),
			  true /* is_struct */));
}

/* Implementation of recording::memento::make_debug_string for
   structs.  */

recording::string *
recording::struct_::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "struct %s", get_name ()->c_str ());
}

/* The implementation of class gcc::jit::recording::union_.  */

/* The constructor for gcc::jit::recording::union_.  */

recording::union_::union_ (context *ctxt,
			   location *loc,
			   string *name)
: compound_type (ctxt, loc, name)
{
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::union_.  */

void
recording::union_::replay_into (replayer *r)
{
  set_playback_obj (
    r->new_compound_type (playback_location (r, get_loc ()),
			  get_name ()->c_str (),
			  false /* is_struct */));
}

/* Implementation of recording::memento::make_debug_string for
   unions.  */

recording::string *
recording::union_::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "union %s", get_name ()->c_str ());
}

/* The implementation of class gcc::jit::recording::fields.  */

/* The constructor for gcc::jit::recording::fields.  */

recording::fields::fields (compound_type *struct_or_union,
			   int num_fields,
			   field **fields)
: memento (struct_or_union->m_ctxt),
  m_struct_or_union (struct_or_union),
  m_fields ()
{
  for (int i = 0; i < num_fields; i++)
    {
      gcc_assert (fields[i]->get_container () == NULL);
      fields[i]->set_container (m_struct_or_union);
      m_fields.safe_push (fields[i]);
    }
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::fields.  */

void
recording::fields::replay_into (replayer *)
{
  vec<playback::field *> playback_fields;
  playback_fields.create (m_fields.length ());
  for (unsigned i = 0; i < m_fields.length (); i++)
    playback_fields.safe_push (m_fields[i]->playback_field ());
  m_struct_or_union->playback_compound_type ()->set_fields (playback_fields);
}

/* Override the default implementation of
   recording::memento::write_to_dump by writing a union/struct
   declaration of this form:

      struct/union NAME {
        TYPE_1 NAME_1;
        TYPE_2 NAME_2;
	....
        TYPE_N NAME_N;
      };

    to the dump.  */

void
recording::fields::write_to_dump (dump &d)
{
  int i;
  field *f;

  d.write ("%s\n{\n", m_struct_or_union->get_debug_string ());
  FOR_EACH_VEC_ELT (m_fields, i, f)
    f->write_to_dump (d);
  d.write ("};\n");
}

/* Implementation of recording::memento::make_debug_string for
   field tables.  */

recording::string *
recording::fields::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "fields");
}

/* The implementation of class gcc::jit::recording::rvalue.  */

/* Create a recording::access_field_rvalue instance and add it to
   the rvalue's context's list of mementos.

   Implements the post-error-checking part of
   gcc_jit_rvalue_access_field.  */

recording::rvalue *
recording::rvalue::access_field (recording::location *loc,
				 field *field)
{
  recording::rvalue *result =
    new access_field_rvalue (m_ctxt, loc, this, field);
  m_ctxt->record (result);
  return result;
}

/* Create a recording::dereference_field_rvalue instance and add it to
   the rvalue's context's list of mementos.

   Implements the post-error-checking part of
   gcc_jit_rvalue_dereference_field.  */

recording::lvalue *
recording::rvalue::dereference_field (recording::location *loc,
				      field *field)
{
  recording::lvalue *result =
    new dereference_field_rvalue (m_ctxt, loc, this, field);
  m_ctxt->record (result);
  return result;
}

/* Create a recording::dereference_rvalue instance and add it to the
   rvalue's context's list of mementos.

   Implements the post-error-checking part of
   gcc_jit_rvalue_dereference.  */

recording::lvalue *
recording::rvalue::dereference (recording::location *loc)
{
  recording::lvalue *result =
    new dereference_rvalue (m_ctxt, loc, this);
  m_ctxt->record (result);
  return result;
}

/* The implementation of class gcc::jit::recording::lvalue.  */

/* Create a recording::new_access_field_of_lvalue instance and add it to
   the lvalue's context's list of mementos.

   Implements the post-error-checking part of
   gcc_jit_lvalue_access_field.  */

recording::lvalue *
recording::lvalue::access_field (recording::location *loc,
				 field *field)
{
  recording::lvalue *result =
    new access_field_of_lvalue (m_ctxt, loc, this, field);
  m_ctxt->record (result);
  return result;
}

/* Create a recording::get_address_of_lvalue instance and add it to
   the lvalue's context's list of mementos.

   Implements the post-error-checking part of
   gcc_jit_lvalue_get_address.  */

recording::rvalue *
recording::lvalue::get_address (recording::location *loc)
{
  recording::rvalue *result =
    new get_address_of_lvalue (m_ctxt, loc, this);
  m_ctxt->record (result);
  return result;
}

/* The implementation of class gcc::jit::recording::param.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::param.  */

void
recording::param::replay_into (replayer *r)
{
  set_playback_obj (r->new_param (playback_location (r, m_loc),
				  m_type->playback_type (),
				  m_name->c_str ()));
}


/* The implementation of class gcc::jit::recording::function.  */

/* gcc::jit::recording::function's constructor.  */

recording::function::function (context *ctxt,
			       recording::location *loc,
			       enum gcc_jit_function_kind kind,
			       type *return_type,
			       recording::string *name,
			       int num_params,
			       recording::param **params,
			       int is_variadic,
			       enum built_in_function builtin_id)
: memento (ctxt),
  m_loc (loc),
  m_kind (kind),
  m_return_type (return_type),
  m_name (name),
  m_params (),
  m_is_variadic (is_variadic),
  m_builtin_id (builtin_id),
  m_locals (),
  m_blocks ()
{
  for (int i = 0; i< num_params; i++)
    m_params.safe_push (params[i]);
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::function.  */

void
recording::function::replay_into (replayer *r)
{
  /* Convert m_params to a vec of playback param.  */
  vec <playback::param *> params;
  int i;
  recording::param *param;
  params.create (m_params.length ());
  FOR_EACH_VEC_ELT (m_params, i, param)
    params.safe_push (param->playback_param ());

  set_playback_obj (r->new_function (playback_location (r, m_loc),
				     m_kind,
				     m_return_type->playback_type (),
				     m_name->c_str (),
				     &params,
				     m_is_variadic,
				     m_builtin_id));
}

/* Create a recording::local instance and add it to
   the functions's context's list of mementos, and to the function's
   list of locals.

   Implements the post-error-checking part of
   gcc_jit_function_new_local.  */

recording::lvalue *
recording::function::new_local (recording::location *loc,
				type *type,
				const char *name)
{
  local *result = new local (this, loc, type, new_string (name));
  m_ctxt->record (result);
  m_locals.safe_push (result);
  return result;
}

/* Create a recording::block instance and add it to
   the functions's context's list of mementos, and to the function's
   list of blocks.

   Implements the post-error-checking part of
   gcc_jit_function_new_block.  */

recording::block*
recording::function::new_block (const char *name)
{
  gcc_assert (m_kind != GCC_JIT_FUNCTION_IMPORTED);

  recording::block *result =
    new recording::block (this, m_blocks.length (), new_string (name));
  m_ctxt->record (result);
  m_blocks.safe_push (result);
  return result;
}

/* Override the default implementation of
   recording::memento::write_to_dump by dumping a C-like
   representation of the function; either like a prototype
   for GCC_JIT_FUNCTION_IMPORTED, or like a full definition for
   all other kinds of function.  */

void
recording::function::write_to_dump (dump &d)
{
  switch (m_kind)
    {
    default: gcc_unreachable ();
    case GCC_JIT_FUNCTION_EXPORTED:
    case GCC_JIT_FUNCTION_IMPORTED:
      d.write ("extern ");
      break;
    case GCC_JIT_FUNCTION_INTERNAL:
      d.write ("static ");
      break;
    case GCC_JIT_FUNCTION_ALWAYS_INLINE:
      d.write ("static inline ");
      break;
     }
  d.write ("%s\n", m_return_type->get_debug_string ());

  if (d.update_locations ())
    m_loc = d.make_location ();

  d.write ("%s (", get_debug_string ());

  int i;
  recording::param *param;
  FOR_EACH_VEC_ELT (m_params, i, param)
    {
      if (i > 0)
	d.write (", ");
      d.write ("%s %s",
	       param->get_type ()->get_debug_string (),
	       param->get_debug_string ());
    }
  d.write (")");
  if (m_kind == GCC_JIT_FUNCTION_IMPORTED)
    {
      d.write ("; /* (imported) */\n\n");
    }
  else
    {
      int i;
      local *var = NULL;
      block *b;
      d.write ("\n{\n");

      /* Write locals: */
      FOR_EACH_VEC_ELT (m_locals, i, var)
	var->write_to_dump (d);
      if (m_locals.length ())
	d.write ("\n");

      /* Write each block: */
      FOR_EACH_VEC_ELT (m_blocks, i, b)
	{
	  if (i > 0)
	    d.write ("\n");
	  b->write_to_dump (d);
	}

      d.write ("}\n\n");
    }
}

/* Pre-compilation validation of a function, for those things we can't
   check until the context is (supposedly) fully-populated.  */

void
recording::function::validate ()
{
  /* Complain about empty functions with non-void return type.  */
  if (m_kind != GCC_JIT_FUNCTION_IMPORTED
      && m_return_type != m_ctxt->get_type (GCC_JIT_TYPE_VOID))
    if (0 == m_blocks.length ())
      m_ctxt->add_error (m_loc,
			 "function %s returns non-void (type: %s)"
			 " but has no blocks",
			 get_debug_string (),
			 m_return_type->get_debug_string ());

  /* Check that all blocks are terminated.  */
  int num_invalid_blocks = 0;
  {
    int i;
    block *b;

    FOR_EACH_VEC_ELT (m_blocks, i, b)
      if (!b->validate ())
	num_invalid_blocks++;
  }

  /* Check that all blocks are reachable.  */
  if (m_blocks.length () > 0 && 0 == num_invalid_blocks)
    {
      /* Iteratively walk the graph of blocks, marking their "m_is_reachable"
	 flag, starting at the initial block.  */
      auto_vec<block *> worklist (m_blocks.length ());
      worklist.safe_push (m_blocks[0]);
      while (worklist.length () > 0)
	{
	  block *b = worklist.pop ();
	  b->m_is_reachable = true;

	  /* Add successor blocks that aren't yet marked to the worklist.  */
	  /* We checked that each block has a terminating statement above .  */
	  block *next1, *next2;
	  int n = b->get_successor_blocks (&next1, &next2);
	  switch (n)
	    {
	    default:
	      gcc_unreachable ();
	    case 2:
	      if (!next2->m_is_reachable)
		worklist.safe_push (next2);
	      /* fallthrough */
	    case 1:
	      if (!next1->m_is_reachable)
		worklist.safe_push (next1);
	      break;
	    case 0:
	      break;
	    }
	}

      /* Now complain about any blocks that haven't been marked.  */
      {
	int i;
	block *b;
	FOR_EACH_VEC_ELT (m_blocks, i, b)
	  if (!b->m_is_reachable)
	    m_ctxt->add_error (b->get_loc (),
			       "unreachable block: %s",
			       b->get_debug_string ());
      }
    }
}

/* Implements the post-error-checking part of
   gcc_jit_function_dump_to_dot.  */

void
recording::function::dump_to_dot (const char *path)
{
  FILE *fp  = fopen (path, "w");
  if (!fp)
    return;

  pretty_printer the_pp;
  the_pp.buffer->stream = fp;

  pretty_printer *pp = &the_pp;

  pp_printf (pp,
	     "digraph %s {\n", get_debug_string ());

  /* Blocks: */
  {
    int i;
    block *b;
    FOR_EACH_VEC_ELT (m_blocks, i, b)
      b->dump_to_dot (pp);
  }

  /* Edges: */
  {
    int i;
    block *b;
    FOR_EACH_VEC_ELT (m_blocks, i, b)
      b->dump_edges_to_dot (pp);
  }

  pp_printf (pp, "}\n");
  pp_flush (pp);
  fclose (fp);
}

/* Implementation of recording::memento::make_debug_string for
   functions.  */

recording::string *
recording::function::make_debug_string ()
{
  return m_name;
}

/* The implementation of class gcc::jit::recording::block.  */

/* Create a recording::eval instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the post-error-checking part of
   gcc_jit_block_add_eval.  */

void
recording::block::add_eval (recording::location *loc,
			    recording::rvalue *rvalue)
{
  statement *result = new eval (this, loc, rvalue);
  m_ctxt->record (result);
  m_statements.safe_push (result);
}

/* Create a recording::assignment instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the post-error-checking part of
   gcc_jit_block_add_assignment.  */

void
recording::block::add_assignment (recording::location *loc,
				  recording::lvalue *lvalue,
				  recording::rvalue *rvalue)
{
  statement *result = new assignment (this, loc, lvalue, rvalue);
  m_ctxt->record (result);
  m_statements.safe_push (result);
}

/* Create a recording::assignment_op instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the post-error-checking part of
   gcc_jit_block_add_assignment_op.  */

void
recording::block::add_assignment_op (recording::location *loc,
				     recording::lvalue *lvalue,
				     enum gcc_jit_binary_op op,
				     recording::rvalue *rvalue)
{
  statement *result = new assignment_op (this, loc, lvalue, op, rvalue);
  m_ctxt->record (result);
  m_statements.safe_push (result);
}

/* Create a recording::comment instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the post-error-checking part of
   gcc_jit_block_add_comment.  */

void
recording::block::add_comment (recording::location *loc,
			       const char *text)
{
  statement *result = new comment (this, loc, new_string (text));
  m_ctxt->record (result);
  m_statements.safe_push (result);
}

/* Create a recording::end_with_conditional instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the post-error-checking part of
   gcc_jit_block_end_with_conditional.  */

void
recording::block::end_with_conditional (recording::location *loc,
					recording::rvalue *boolval,
					recording::block *on_true,
					recording::block *on_false)
{
  statement *result = new conditional (this, loc, boolval, on_true, on_false);
  m_ctxt->record (result);
  m_statements.safe_push (result);
  m_has_been_terminated = true;
}

/* Create a recording::end_with_jump instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the post-error-checking part of
   gcc_jit_block_end_with_jump.  */

void
recording::block::end_with_jump (recording::location *loc,
				 recording::block *target)
{
  statement *result = new jump (this, loc, target);
  m_ctxt->record (result);
  m_statements.safe_push (result);
  m_has_been_terminated = true;
}

/* Create a recording::end_with_return instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the post-error-checking parts of
   gcc_jit_block_end_with_return and
   gcc_jit_block_end_with_void_return.  */

void
recording::block::end_with_return (recording::location *loc,
				   recording::rvalue *rvalue)
{
  /* This is used by both gcc_jit_function_add_return and
     gcc_jit_function_add_void_return; rvalue will be non-NULL for
     the former and NULL for the latter.  */
  statement *result = new return_ (this, loc, rvalue);
  m_ctxt->record (result);
  m_statements.safe_push (result);
  m_has_been_terminated = true;
}

/* Override the default implementation of
   recording::memento::write_to_dump for blocks by writing
   an unindented block name as a label, followed by the indented
   statements:

    BLOCK_NAME:
      STATEMENT_1;
      STATEMENT_2;
      ...
      STATEMENT_N;  */

void
recording::block::write_to_dump (dump &d)
{
  d.write ("%s:\n", get_debug_string ());

  int i;
  statement *s;
  FOR_EACH_VEC_ELT (m_statements, i, s)
    s->write_to_dump (d);
}

/* Validate a block by ensuring that it has been terminated.  */

bool
recording::block::validate ()
{
  if (!has_been_terminated ())
    {
      statement *stmt = get_last_statement ();
      location *loc = stmt ? stmt->get_loc () : NULL;
      m_func->get_context ()->add_error (loc,
					 "unterminated block in %s: %s",
					 m_func->get_debug_string (),
					 get_debug_string ());
      return false;
    }

  return true;
}

/* Get the source-location of a block by using that of the first
   statement within it, if any.  */

recording::location *
recording::block::get_loc () const
{
  recording::statement *stmt = get_first_statement ();
  if (stmt)
    return stmt->get_loc ();
  else
    return NULL;
}

/* Get the first statement within a block, if any.  */

recording::statement *
recording::block::get_first_statement () const
{
  if (m_statements.length ())
    return m_statements[0];
  else
    return NULL;
}

/* Get the last statement within a block, if any.  */

recording::statement *
recording::block::get_last_statement () const
{
  if (m_statements.length ())
    return m_statements[m_statements.length () - 1];
  else
    return NULL;
}

/* Assuming that this block has been terminated, get the number of
   successor blocks, which will be 0, 1 or 2, for return, unconditional
   jump, and conditional jump respectively.
   NEXT1 and NEXT2 must be non-NULL.  The first successor block (if any)
   is written to NEXT1, and the second (if any) to NEXT2.

   Used when validating functions, and when dumping dot representations
   of them.  */

int
recording::block::get_successor_blocks (block **next1, block **next2) const
{
  gcc_assert (m_has_been_terminated);
  gcc_assert (next1);
  gcc_assert (next2);
  statement *last_statement = get_last_statement ();
  gcc_assert (last_statement);
  return last_statement->get_successor_blocks (next1, next2);
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::block.  */

void
recording::block::replay_into (replayer *)
{
  set_playback_obj (m_func->playback_function ()
		      ->new_block (playback_string (m_name)));
}

/* Implementation of recording::memento::make_debug_string for
   blocks.  */

recording::string *
recording::block::make_debug_string ()
{
  if (m_name)
    return m_name;
  else
    return string::from_printf (m_ctxt,
				"<UNNAMED BLOCK %p>",
				(void *)this);
}

/* Dump a block in graphviz form into PP, capturing the block name (if
   any) and the statements.  */

void
recording::block::dump_to_dot (pretty_printer *pp)
{
  pp_printf (pp,
	     ("\tblock_%d "
	      "[shape=record,style=filled,fillcolor=white,label=\"{"),
	     m_index);
  pp_write_text_to_stream (pp);
  if (m_name)
    {
      pp_string (pp, m_name->c_str ());
      pp_string (pp, ":");
      pp_newline (pp);
      pp_write_text_as_dot_label_to_stream (pp, true /*for_record*/);
    }

  int i;
  statement *s;
  FOR_EACH_VEC_ELT (m_statements, i, s)
    {
      pp_string (pp, s->get_debug_string ());
      pp_newline (pp);
      pp_write_text_as_dot_label_to_stream (pp, true /*for_record*/);
    }

  pp_printf (pp,
	     "}\"];\n\n");
  pp_flush (pp);
}

/* Dump the out-edges of the block in graphviz form into PP.  */

void
recording::block::dump_edges_to_dot (pretty_printer *pp)
{
  block *next[2];
  int num_succs = get_successor_blocks (&next[0], &next[1]);
  for (int i = 0; i < num_succs; i++)
    pp_printf (pp,
	       "\tblock_%d:s -> block_%d:n;\n",
	       m_index, next[i]->m_index);
}

/* The implementation of class gcc::jit::recording::global.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::global.  */

void
recording::global::replay_into (replayer *r)
{
  set_playback_obj (r->new_global (playback_location (r, m_loc),
				   m_type->playback_type (),
				   playback_string (m_name)));
}

/* The implementation of class gcc::jit::recording::memento_of_new_rvalue_from_int.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::memento_of_new_rvalue_from_int.  */

void
recording::memento_of_new_rvalue_from_int::replay_into (replayer *r)
{
  set_playback_obj (r->new_rvalue_from_int (m_type->playback_type (),
					    m_value));
}

/* Implementation of recording::memento::make_debug_string for
   rvalue_from_int, rendering it as
     (TYPE)LITERAL
   e.g.
     "(int)42".  */

recording::string *
recording::memento_of_new_rvalue_from_int::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "(%s)%i",
			      m_type->get_debug_string (),
			      m_value);
}

/* The implementation of class gcc::jit::recording::memento_of_new_rvalue_from_double.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::memento_of_new_rvalue_from_double.  */

void
recording::memento_of_new_rvalue_from_double::replay_into (replayer *r)
{
  set_playback_obj (r->new_rvalue_from_double (m_type->playback_type (),
					       m_value));
}

/* Implementation of recording::memento::make_debug_string for
   rvalue_from_double, rendering it as
     (TYPE)LITERAL
   e.g.
     "(float)42.0".  */

recording::string *
recording::memento_of_new_rvalue_from_double::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "(%s)%f",
			      m_type->get_debug_string (),
			      m_value);
}

/* The implementation of class gcc::jit::recording::memento_of_new_rvalue_from_ptr.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::memento_of_new_rvalue_from_ptr.  */

void
recording::memento_of_new_rvalue_from_ptr::replay_into (replayer *r)
{
  set_playback_obj (r->new_rvalue_from_ptr (m_type->playback_type (),
					    m_value));
}

/* Implementation of recording::memento::make_debug_string for
   rvalue_from_ptr, rendering it as
     (TYPE)HEX
   e.g.
     "(int *)0xdeadbeef"

   Zero is rendered as NULL e.g.
     "(int *)NULL".  */

recording::string *
recording::memento_of_new_rvalue_from_ptr::make_debug_string ()
{
  if (m_value != NULL)
    return string::from_printf (m_ctxt,
				"(%s)%p",
				m_type->get_debug_string (), m_value);
  else
    return string::from_printf (m_ctxt,
				"(%s)NULL",
				m_type->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::memento_of_new_string_literal.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::memento_of_new_string_literal.  */

void
recording::memento_of_new_string_literal::replay_into (replayer *r)
{
  set_playback_obj (r->new_string_literal (m_value->c_str ()));
}

/* Implementation of recording::memento::make_debug_string for
   string literals.  */

recording::string *
recording::memento_of_new_string_literal::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      m_value->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::unary_op.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::unary_op.  */

void
recording::unary_op::replay_into (replayer *r)
{
  set_playback_obj (r->new_unary_op (playback_location (r, m_loc),
				     m_op,
				     get_type ()->playback_type (),
				     m_a->playback_rvalue ()));
}

/* Implementation of recording::memento::make_debug_string for
   unary ops.  */

static const char * const unary_op_strings[] = {
  "-", /* GCC_JIT_UNARY_OP_MINUS */
  "~", /* GCC_JIT_UNARY_OP_BITWISE_NEGATE */
  "!", /* GCC_JIT_UNARY_OP_LOGICAL_NEGATE */
};

recording::string *
recording::unary_op::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "%s(%s)",
			      unary_op_strings[m_op],
			      m_a->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::binary_op.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::binary_op.  */

void
recording::binary_op::replay_into (replayer *r)
{
  set_playback_obj (r->new_binary_op (playback_location (r, m_loc),
				      m_op,
				      get_type ()->playback_type (),
				      m_a->playback_rvalue (),
				      m_b->playback_rvalue ()));
}

/* Implementation of recording::memento::make_debug_string for
   binary ops.  */

static const char * const binary_op_strings[] = {
  "+", /* GCC_JIT_BINARY_OP_PLUS */
  "-", /* GCC_JIT_BINARY_OP_MINUS */
  "*", /* GCC_JIT_BINARY_OP_MULT */
  "/", /* GCC_JIT_BINARY_OP_DIVIDE */
  "%", /* GCC_JIT_BINARY_OP_MODULO */
  "&", /* GCC_JIT_BINARY_OP_BITWISE_AND */
  "^", /* GCC_JIT_BINARY_OP_BITWISE_XOR */
  "|", /* GCC_JIT_BINARY_OP_BITWISE_OR */
  "&&", /* GCC_JIT_BINARY_OP_LOGICAL_AND */
  "||", /* GCC_JIT_BINARY_OP_LOGICAL_OR */
  "<<", /* GCC_JIT_BINARY_OP_LSHIFT */
  ">>", /* GCC_JIT_BINARY_OP_RSHIFT */
};

recording::string *
recording::binary_op::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "%s %s %s",
			      m_a->get_debug_string (),
			      binary_op_strings[m_op],
			      m_b->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::comparison.  */

/* Implementation of recording::memento::make_debug_string for
   comparisons.  */

static const char * const comparison_strings[] =
{
  "==", /* GCC_JIT_COMPARISON_EQ */
  "!=", /* GCC_JIT_COMPARISON_NE */
  "<",  /* GCC_JIT_COMPARISON_LT */
  "<=", /* GCC_JIT_COMPARISON_LE */
  ">",  /* GCC_JIT_COMPARISON_GT */
  ">=", /* GCC_JIT_COMPARISON_GE */
};

recording::string *
recording::comparison::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "%s %s %s",
			      m_a->get_debug_string (),
			      comparison_strings[m_op],
			      m_b->get_debug_string ());
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::comparison.  */

void
recording::comparison::replay_into (replayer *r)
{
  set_playback_obj (r->new_comparison (playback_location (r, m_loc),
				       m_op,
				       m_a->playback_rvalue (),
				       m_b->playback_rvalue ()));
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::cast.  */

void
recording::cast::replay_into (replayer *r)
{
  set_playback_obj (r->new_cast (playback_location (r, m_loc),
				 m_rvalue->playback_rvalue (),
				 get_type ()->playback_type ()));
}

/* Implementation of recording::memento::make_debug_string for
   casts.  */

recording::string *
recording::cast::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "(%s)%s",
			      get_type ()->get_debug_string (),
			      m_rvalue->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::call.  */

/* The constructor for gcc::jit::recording::call.  */

recording::call::call (recording::context *ctxt,
		       recording::location *loc,
		       recording::function *func,
		       int numargs,
		       rvalue **args)
: rvalue (ctxt, loc, func->get_return_type ()),
  m_func (func),
  m_args ()
{
  for (int i = 0; i< numargs; i++)
    m_args.safe_push (args[i]);
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::call.  */

void
recording::call::replay_into (replayer *r)
{
  vec<playback::rvalue *> playback_args;
  playback_args.create (m_args.length ());
  for (unsigned i = 0; i< m_args.length (); i++)
    playback_args.safe_push (m_args[i]->playback_rvalue ());

  set_playback_obj (r->new_call (playback_location (r, m_loc),
				 m_func->playback_function (),
				 playback_args));
}

/* Implementation of recording::memento::make_debug_string for
   function calls.  */

recording::string *
recording::call::make_debug_string ()
{
  /* First, build a buffer for the arguments.  */
  /* Calculate length of said buffer.  */
  size_t sz = 1; /* nil terminator */
  for (unsigned i = 0; i< m_args.length (); i++)
    {
      sz += strlen (m_args[i]->get_debug_string ());
      sz += 2; /* ", " separator */
    }

  /* Now allocate and populate the buffer.  */
  char *argbuf = new char[sz];
  size_t len = 0;

  for (unsigned i = 0; i< m_args.length (); i++)
    {
      strcpy (argbuf + len, m_args[i]->get_debug_string ());
      len += strlen (m_args[i]->get_debug_string ());
      if (i + 1 < m_args.length ())
	{
	  strcpy (argbuf + len, ", ");
	  len += 2;
	}
    }
  argbuf[len] = '\0';

  /* ...and use it to get the string for the call as a whole.  */
  string *result = string::from_printf (m_ctxt,
					"%s (%s)",
					m_func->get_debug_string (),
					argbuf);

  delete[] argbuf;

  return result;
}

/* The implementation of class gcc::jit::recording::call_through_ptr.  */

/* The constructor for recording::call_through_ptr. */

recording::call_through_ptr::call_through_ptr (recording::context *ctxt,
					       recording::location *loc,
					       recording::rvalue *fn_ptr,
					       int numargs,
					       rvalue **args)
: rvalue (ctxt, loc,
	  fn_ptr->get_type ()->dereference ()
	    ->as_a_function_type ()->get_return_type ()),
  m_fn_ptr (fn_ptr),
  m_args ()
{
  for (int i = 0; i< numargs; i++)
    m_args.safe_push (args[i]);
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::call_through_ptr.  */

void
recording::call_through_ptr::replay_into (replayer *r)
{
  vec<playback::rvalue *> playback_args;
  playback_args.create (m_args.length ());
  for (unsigned i = 0; i< m_args.length (); i++)
    playback_args.safe_push (m_args[i]->playback_rvalue ());

  set_playback_obj (r->new_call_through_ptr (playback_location (r, m_loc),
					     m_fn_ptr->playback_rvalue (),
					     playback_args));
}

/* Implementation of recording::memento::make_debug_string for
   calls through function ptrs.  */

recording::string *
recording::call_through_ptr::make_debug_string ()
{
  /* First, build a buffer for the arguments.  */
  /* Calculate length of said buffer.  */
  size_t sz = 1; /* nil terminator */
  for (unsigned i = 0; i< m_args.length (); i++)
    {
      sz += strlen (m_args[i]->get_debug_string ());
      sz += 2; /* ", " separator */
    }

  /* Now allocate and populate the buffer.  */
  char *argbuf = new char[sz];
  size_t len = 0;

  for (unsigned i = 0; i< m_args.length (); i++)
    {
      strcpy (argbuf + len, m_args[i]->get_debug_string ());
      len += strlen (m_args[i]->get_debug_string ());
      if (i + 1 < m_args.length ())
	{
	  strcpy (argbuf + len, ", ");
	  len += 2;
	}
    }
  argbuf[len] = '\0';

  /* ...and use it to get the string for the call as a whole.  */
  string *result = string::from_printf (m_ctxt,
					"%s (%s)",
					m_fn_ptr->get_debug_string (),
					argbuf);

  delete[] argbuf;

  return result;
}

/* The implementation of class gcc::jit::recording::array_access.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::array_access.  */

void
recording::array_access::replay_into (replayer *r)
{
  set_playback_obj (
    r->new_array_access (playback_location (r, m_loc),
			 m_ptr->playback_rvalue (),
			 m_index->playback_rvalue ()));
}

/* Implementation of recording::memento::make_debug_string for
   array accesses.  */

recording::string *
recording::array_access::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "%s[%s]",
			      m_ptr->get_debug_string (),
			      m_index->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::access_field_of_lvalue.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::access_field_of_lvalue.  */

void
recording::access_field_of_lvalue::replay_into (replayer *r)
{
  set_playback_obj (
    m_lvalue->playback_lvalue ()
      ->access_field (playback_location (r, m_loc),
		      m_field->playback_field ()));

}

/* Implementation of recording::memento::make_debug_string for
   accessing a field of an lvalue.  */

recording::string *
recording::access_field_of_lvalue::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "%s.%s",
			      m_lvalue->get_debug_string (),
			      m_field->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::access_field_rvalue.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::access_field_rvalue.  */

void
recording::access_field_rvalue::replay_into (replayer *r)
{
  set_playback_obj (
    m_rvalue->playback_rvalue ()
      ->access_field (playback_location (r, m_loc),
		      m_field->playback_field ()));
}

/* Implementation of recording::memento::make_debug_string for
   accessing a field of an rvalue.  */

recording::string *
recording::access_field_rvalue::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "%s.%s",
			      m_rvalue->get_debug_string (),
			      m_field->get_debug_string ());
}

/* The implementation of class
   gcc::jit::recording::dereference_field_rvalue.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::dereference_field_rvalue.  */

void
recording::dereference_field_rvalue::replay_into (replayer *r)
{
  set_playback_obj (
    m_rvalue->playback_rvalue ()->
      dereference_field (playback_location (r, m_loc),
			 m_field->playback_field ()));
}

/* Implementation of recording::memento::make_debug_string for
   dereferencing a field of an rvalue.  */

recording::string *
recording::dereference_field_rvalue::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "%s->%s",
			      m_rvalue->get_debug_string (),
			      m_field->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::dereference_rvalue.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::dereference_rvalue.  */

void
recording::dereference_rvalue::replay_into (replayer *r)
{
  set_playback_obj (
    m_rvalue->playback_rvalue ()->
      dereference (playback_location (r, m_loc)));
}

/* Implementation of recording::memento::make_debug_string for
   dereferencing an rvalue.  */

recording::string *
recording::dereference_rvalue::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "*%s",
			      m_rvalue->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::get_address_of_lvalue.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::get_address_of_lvalue.  */

void
recording::get_address_of_lvalue::replay_into (replayer *r)
{
  set_playback_obj (
    m_lvalue->playback_lvalue ()->
      get_address (playback_location (r, m_loc)));
}

/* Implementation of recording::memento::make_debug_string for
   getting the address of an lvalue.  */

recording::string *
recording::get_address_of_lvalue::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "&%s",
			      m_lvalue->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::local.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::local.  */

void
recording::local::replay_into (replayer *r)
{
  set_playback_obj (
    m_func->playback_function ()
      ->new_local (playback_location (r, m_loc),
		   m_type->playback_type (),
		   playback_string (m_name)));
}

/* Override the default implementation of
   recording::memento::write_to_dump for locals by writing
      TYPE NAME;
   for use at the top of the function body as if it were a
   declaration.  */

void
recording::local::write_to_dump (dump &d)
{
  if (d.update_locations ())
    m_loc = d.make_location ();
  d.write("  %s %s;\n",
	  m_type->get_debug_string (),
	  get_debug_string ());
}

/* The implementation of class gcc::jit::recording::statement.  */

/* We poison the default implementation of
   gcc::jit::recording::statement::get_successor_blocks
   since this vfunc must only ever be called on terminator
   statements.  */

int
recording::statement::get_successor_blocks (block **/*out_next1*/,
					    block **/*out_next2*/) const
{
  /* The base class implementation is for non-terminating statements,
     and thus should never be called.  */
  gcc_unreachable ();
  return 0;
}

/* Extend the default implementation of
   recording::memento::write_to_dump for statements by (if requested)
   updating the location of the statement to the current location in
   the dumpfile.  */

void
recording::statement::write_to_dump (dump &d)
{
  memento::write_to_dump (d);
  if (d.update_locations ())
    m_loc = d.make_location ();
}

/* The implementation of class gcc::jit::recording::eval.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::eval.  */

void
recording::eval::replay_into (replayer *r)
{
  playback_block (get_block ())
    ->add_eval (playback_location (r),
		m_rvalue->playback_rvalue ());
}

/* Implementation of recording::memento::make_debug_string for
   an eval statement.  */

recording::string *
recording::eval::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "(void)%s;",
			      m_rvalue->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::assignment.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::assignment.  */

void
recording::assignment::replay_into (replayer *r)
{
  playback_block (get_block ())
    ->add_assignment (playback_location (r),
		      m_lvalue->playback_lvalue (),
		      m_rvalue->playback_rvalue ());
}

/* Implementation of recording::memento::make_debug_string for
   an assignment statement.  */

recording::string *
recording::assignment::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "%s = %s;",
			      m_lvalue->get_debug_string (),
			      m_rvalue->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::assignment_op.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::assignment_op.  */

void
recording::assignment_op::replay_into (replayer *r)
{
  playback::type *result_type =
    m_lvalue->playback_lvalue ()->get_type ();

  playback::rvalue *binary_op =
    r->new_binary_op (playback_location (r),
		      m_op,
		      result_type,
		      m_lvalue->playback_rvalue (),
		      m_rvalue->playback_rvalue ());

  playback_block (get_block ())
    ->add_assignment (playback_location (r),
		      m_lvalue->playback_lvalue (),
		      binary_op);
}

/* Implementation of recording::memento::make_debug_string for
   an assignment_op statement.  */

recording::string *
recording::assignment_op::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "%s %s= %s;",
			      m_lvalue->get_debug_string (),
			      binary_op_strings[m_op],
			      m_rvalue->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::comment.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::comment.  */

void
recording::comment::replay_into (replayer *r)
{
  playback_block (get_block ())
    ->add_comment (playback_location (r),
		   m_text->c_str ());
}

/* Implementation of recording::memento::make_debug_string for
   a comment "statement".  */

recording::string *
recording::comment::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "/* %s */",
			      m_text->c_str ());
}

/* The implementation of class gcc::jit::recording::conditional.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::conditional.  */

void
recording::conditional::replay_into (replayer *r)
{
  playback_block (get_block ())
    ->add_conditional (playback_location (r),
		       m_boolval->playback_rvalue (),
		       playback_block (m_on_true),
		       playback_block (m_on_false));
}

/* Override the poisoned default implementation of
   gcc::jit::recording::statement::get_successor_blocks

   A conditional jump has 2 successor blocks.  */

int
recording::conditional::get_successor_blocks (block **out_next1,
					      block **out_next2) const
{
  *out_next1 = m_on_true;
  *out_next2 = m_on_false;
  return 2;
}

/* Implementation of recording::memento::make_debug_string for
   a conditional jump statement.  */

recording::string *
recording::conditional::make_debug_string ()
{
  if (m_on_false)
    return string::from_printf (m_ctxt,
				"if (%s) goto %s; else goto %s;",
				m_boolval->get_debug_string (),
				m_on_true->get_debug_string (),
				m_on_false->get_debug_string ());
  else
    return string::from_printf (m_ctxt,
				"if (%s) goto %s;",
				m_boolval->get_debug_string (),
				m_on_true->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::jump.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::jump.  */

void
recording::jump::replay_into (replayer *r)
{
  playback_block (get_block ())
    ->add_jump (playback_location (r),
		m_target->playback_block ());
}

/* Override the poisoned default implementation of
   gcc::jit::recording::statement::get_successor_blocks

   An unconditional jump has 1 successor block.  */

int
recording::jump::get_successor_blocks (block **out_next1,
				       block **/*out_next2*/) const
{
  *out_next1 = m_target;
  return 1;
}

/* Implementation of recording::memento::make_debug_string for
   a unconditional jump statement.  */

recording::string *
recording::jump::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "goto %s;",
			      m_target->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::return_.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::return_.  */

void
recording::return_::replay_into (replayer *r)
{
  playback_block (get_block ())
    ->add_return (playback_location (r),
		  m_rvalue ? m_rvalue->playback_rvalue () : NULL);
}

/* Override the poisoned default implementation of
   gcc::jit::recording::statement::get_successor_blocks

   A return statement has no successor block.  */

int
recording::return_::get_successor_blocks (block **/*out_next1*/,
					  block **/*out_next2*/) const
{
  return 0;
}

/* Implementation of recording::memento::make_debug_string for
   a return statement (covers both those with and without rvalues).  */

recording::string *
recording::return_::make_debug_string ()
{
  if (m_rvalue)
    return string::from_printf (m_ctxt,
				"return %s;",
				m_rvalue->get_debug_string ());
  else
    return string::from_printf (m_ctxt,
				"return;");
}

} // namespace gcc::jit

} // namespace gcc
