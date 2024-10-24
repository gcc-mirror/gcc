/* Internals of libgccjit: classes for recording calls made to the JIT API.
   Copyright (C) 2013-2024 Free Software Foundation, Inc.
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
#define INCLUDE_MEMORY
#define INCLUDE_SSTREAM
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "pretty-print.h"
#include "toplev.h"


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
  int len;
  va_list ap;
  char *buf;

  /* If there was an error opening the file, we've already reported it.
     Don't attempt further work.  */
  if (!m_file)
    return;

  va_start (ap, fmt);
  len = vasprintf (&buf, fmt, ap);
  va_end (ap);

  if (buf == NULL || len < 0)
    {
      m_ctxt.add_error (NULL, "malloc failure writing to dumpfile %s",
			m_filename);
      return;
    }

  if (fwrite (buf, strlen (buf), 1, m_file) != 1)
    m_ctxt.add_error (NULL, "error writing to dump file %s",
		      m_filename);

  /* Flush after each line, to ease debugging crashes.  */
  fflush (m_file);

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
  return m_ctxt.new_location (m_filename, m_line, m_column,
			      /* We need to flag such locations as *not*
				 created by the user, so that
				 reproducer::get_identifier can cope with
				 them appearing *after* the memento that
				 refers to them.  */
			      false);
}

/* A collection of allocations, all of which can be released together, to
   avoid needing to track and release them individually.  */

class allocator
{
 public:
  ~allocator ();

  char *
  xstrdup_printf (const char *, ...)
    ATTRIBUTE_RETURNS_NONNULL
    GNU_PRINTF(2, 3);

  char *
  xstrdup_printf_va (const char *, va_list ap)
    ATTRIBUTE_RETURNS_NONNULL
    GNU_PRINTF(2, 0);

 private:
  auto_vec <void *> m_buffers;
};

/* allocator's destructor.  Call "free" on all of the allocations.  */

allocator::~allocator ()
{
  unsigned i;
  void *buffer;
  FOR_EACH_VEC_ELT (m_buffers, i, buffer)
    free (buffer);
}

/* Formatted printing, allocating to a buffer (or exiting the process if
   the allocation fails).

   The buffer exists until the allocator is cleaned up, and is freed at
   that point, so the caller doesn't need to track the result.  */

char *
allocator::xstrdup_printf (const char *fmt, ...)
{
  char *result;
  va_list ap;
  va_start (ap, fmt);
  result = xstrdup_printf_va (fmt, ap);
  va_end (ap);
  return result;
}

/* Formatted printing, allocating to a buffer (or exiting the process if
   the allocation fails).

   The buffer exists until the allocator is cleaned up, and is freed at
   that point, so the caller doesn't need to track the result.  */

char *
allocator::xstrdup_printf_va (const char *fmt, va_list ap)
{
  char *result = xvasprintf (fmt, ap);
  m_buffers.safe_push (result);
  return result;
}

/* gcc::jit::reproducer is a subclass of gcc::jit::dump, used for
   implementing gcc_jit_context_dump_reproducer_to_file.  */

class reproducer : public dump
{
 public:
  reproducer (recording::context &ctxt,
	      const char *filename);

  void
  write_params (const vec <recording::context *> &contexts);

  void
  write_args (const vec <recording::context *> &contexts);

  const char *
  make_identifier (recording::memento *m, const char *prefix);

  const char *
  make_tmp_identifier (const char *prefix, recording::memento *m);

  const char *
  get_identifier (recording::context *ctxt);

  const char *
  get_identifier (recording::memento *m);

  const char *
  get_identifier_as_rvalue (recording::rvalue *m);

  const char *
  get_identifier_as_lvalue (recording::lvalue *m);

  const char *
  get_identifier_as_type (recording::type *m);

  char *
  xstrdup_printf (const char *, ...)
    ATTRIBUTE_RETURNS_NONNULL
    GNU_PRINTF(2, 3);

 private:
  const char * ensure_identifier_is_unique (const char *candidate, void *ptr);

 private:
  hash_map<recording::memento *, const char *> m_map_memento_to_identifier;

  struct hash_traits : public string_hash
  {
    static void remove (const char *) {}
  };
  hash_set<const char *, false, hash_traits> m_set_identifiers;
  allocator m_allocator;
};

/* gcc::jit::reproducer's constructor.  */

reproducer::reproducer (recording::context &ctxt,
			const char *filename) :
  dump (ctxt, filename, 0),
  m_map_memento_to_identifier (),
  m_set_identifiers (),
  m_allocator ()
{
}

/* Write out a list of contexts as a set of parameters within a
   C function declaration.  */

void
reproducer::write_params (const vec <recording::context *> &contexts)
{
  unsigned i;
  recording::context *ctxt;
  FOR_EACH_VEC_ELT (contexts, i, ctxt)
    {
      write ("gcc_jit_context *%s",
	     get_identifier (ctxt));
      if (i < contexts.length () - 1)
	write (",\n"
	       "             ");
    }
}

/* Write out a list of contexts as a set of arguments within a call
   to a C function.  */

void
reproducer::write_args (const vec <recording::context *> &contexts)
{
  unsigned i;
  recording::context *ctxt;
  FOR_EACH_VEC_ELT (contexts, i, ctxt)
    {
      write ("%s",
	     get_identifier (ctxt));
      if (i < contexts.length () - 1)
	write (",\n"
	       "               ");
    }
}

/* Ensure that STR is a valid C identifier by overwriting
   any invalid chars in-place with underscores.

   This doesn't special-case the first character.  */

static void
convert_to_identifier (char *str)
{
  for (char *p = str; *p; p++)
    if (!ISALNUM (*p))
      *p = '_';
}

/* Given CANDIDATE, a possible C identifier for use in a reproducer,
   ensure that it is unique within the generated source file by
   appending PTR to it if necessary.  Return the resulting string.

   The reproducer will eventually clean up the buffer in its dtor.  */

const char *
reproducer::ensure_identifier_is_unique (const char *candidate, void *ptr)
{
  if (m_set_identifiers.contains (candidate))
    candidate = m_allocator.xstrdup_printf ("%s_%p", candidate, ptr);
  gcc_assert (!m_set_identifiers.contains (candidate));
  m_set_identifiers.add (candidate);
  return candidate;
}

/* Generate a C identifier for the given memento, associating the generated
   buffer with the memento (for future calls to get_identifier et al).

   The reproducer will eventually clean up the buffer in its dtor.  */
const char *
reproducer::make_identifier (recording::memento *m, const char *prefix)
{
  const char *result;
  if (strlen (m->get_debug_string ()) < 100)
    {
      char *buf = m_allocator.xstrdup_printf ("%s_%s",
					      prefix,
					      m->get_debug_string ());
      convert_to_identifier (buf);
      result = buf;
    }
  else
    result = m_allocator.xstrdup_printf ("%s_%p",
					 prefix, (void *) m);
  result = ensure_identifier_is_unique (result, m);
  m_map_memento_to_identifier.put (m, result);
  return result;
}

/* Generate a C identifier for a temporary variable.
   The reproducer will eventually clean up the buffer in its dtor.  */

const char *
reproducer::make_tmp_identifier (const char *prefix, recording::memento *m)
{
  return m_allocator.xstrdup_printf ("%s_%s",
				     prefix, get_identifier (m));
}

/* Generate a C identifier for the given context.
   The reproducer will eventually clean up the buffer in its dtor.  */

const char *
reproducer::get_identifier (recording::context *ctxt)
{
  return m_allocator.xstrdup_printf ("ctxt_%p",
				     (void *)ctxt);
}

/* Locate the C identifier for the given memento, which is assumed to
   have already been created via make_identifier.  */

const char *
reproducer::get_identifier (recording::memento *m)
{
  if (!m)
    return "NULL";

  /* gcc_jit_context_dump_to_file (, , 1) generates and writes locations,
     and hence these locations appear in the context's memento list
     out-of-order: they appear in the context's memento list *after*
     the memento that refers to them.  For this case, it's simplest to
     pretend that they're NULL when writing out the code to recreate the
     memento that uses them.  */
  if (recording::location *loc = m->dyn_cast_location ())
    if (!loc->created_by_user ())
      return "NULL";

  const char **slot = m_map_memento_to_identifier.get (m);
  if (!slot)
    {
      get_context ().add_error (NULL,
				"unable to find identifier for %p: %s",
				(void *)m,
				m->get_debug_string ());
      gcc_unreachable ();
    }
  return *slot;
}

/* Locate the C identifier for the given rvalue, wrapping it within
   a gcc_*_as_rvalue upcast if necessary.  */

const char *
reproducer::get_identifier_as_rvalue (recording::rvalue *m)
{
  return m->access_as_rvalue (*this);
}

/* Locate the C identifier for the given lvalue, wrapping it within
   a gcc_*_as_lvalue upcast if necessary.  */

const char *
reproducer::get_identifier_as_lvalue (recording::lvalue *m)
{
  return m->access_as_lvalue (*this);
}

/* Locate the C identifier for the given type, wrapping it within
   a gcc_*_as_type upcast if necessary.  */

const char *
reproducer::get_identifier_as_type (recording::type *m)
{
  return m->access_as_type (*this);
}

/* Formatted printing, allocating to a buffer (or exiting the process if
   the allocation fails).

   The buffer exists until the allocator is cleaned up, and is freed at
   that point, so the caller doesn't need to track the result.

   Note that we can't use ggc_printf since we're not within the compiler
   proper (when within gcc_jit_context_dump_reproducer_to_file).  */

char *
reproducer::xstrdup_printf (const char *fmt, ...)
{
  char *result;
  va_list ap;
  va_start (ap, fmt);
  result = m_allocator.xstrdup_printf_va (fmt, ap);
  va_end (ap);
  return result;
}

/* A helper class for implementing make_debug_string, for building
   a temporary string from a vec of rvalues.  */

class comma_separated_string
{
 public:
  comma_separated_string (const auto_vec<recording::rvalue *> &rvalues,
			  enum recording::precedence prec);
  ~comma_separated_string ();

  const char *as_char_ptr () const { return m_buf; }

 private:
  char *m_buf;
};

/* comma_separated_string's ctor
   Build m_buf.  */

comma_separated_string::comma_separated_string
  (const auto_vec<recording::rvalue *> &rvalues,
   enum recording::precedence prec)
: m_buf (NULL)
{
  /* Calculate length of said buffer.  */
  size_t sz = 1; /* nil terminator */
  for (unsigned i = 0; i< rvalues.length (); i++)
    {
      sz += strlen (rvalues[i]->get_debug_string_parens (prec));
      sz += 2; /* ", " separator */
    }

  /* Now allocate and populate the buffer.  */
  m_buf = new char[sz];
  size_t len = 0;

  for (unsigned i = 0; i< rvalues.length (); i++)
    {
      strcpy (m_buf + len, rvalues[i]->get_debug_string_parens (prec));
      len += strlen (rvalues[i]->get_debug_string_parens (prec));
      if (i + 1 < rvalues.length ())
	{
	  strcpy (m_buf + len, ", ");
	  len += 2;
	}
    }
  m_buf[len] = '\0';
}

/* comma_separated_string's dtor.  */

comma_separated_string::~comma_separated_string ()
{
  delete[] m_buf;
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
  : log_user (NULL),
    m_parent_ctxt (parent_ctxt),
    m_toplevel_ctxt (m_parent_ctxt ? m_parent_ctxt->m_toplevel_ctxt : this),
    m_timer (NULL),
    m_error_count (0),
    m_first_error_str (NULL),
    m_owns_first_error_str (false),
    m_last_error_str (NULL),
    m_owns_last_error_str (false),
    m_mementos (),
    m_compound_types (),
    m_globals (),
    m_functions (),
    m_FILE_type (NULL),
    m_builtins_manager(NULL)
{
  if (parent_ctxt)
    {
      /* Inherit options from parent.  */
      for (unsigned i = 0; i < ARRAY_SIZE (m_str_options); i++)
	{
	  const char *parent_opt = parent_ctxt->m_str_options[i];
	  m_str_options[i] = parent_opt ? xstrdup (parent_opt) : NULL;
	}
      memcpy (m_int_options,
	      parent_ctxt->m_int_options,
	      sizeof (m_int_options));
      memcpy (m_bool_options,
	      parent_ctxt->m_bool_options,
	      sizeof (m_bool_options));
      memcpy (m_inner_bool_options,
	      parent_ctxt->m_inner_bool_options,
	      sizeof (m_inner_bool_options));
      set_logger (parent_ctxt->get_logger ());
    }
  else
    {
      memset (m_str_options, 0, sizeof (m_str_options));
      memset (m_int_options, 0, sizeof (m_int_options));
      memset (m_bool_options, 0, sizeof (m_bool_options));
      memset (m_inner_bool_options, 0, sizeof (m_inner_bool_options));
      m_inner_bool_options[INNER_BOOL_OPTION_PRINT_ERRORS_TO_STDERR] = true;
    }

  memset (m_basic_types, 0, sizeof (m_basic_types));
}

/* The destructor for gcc::jit::recording::context, implicitly used by
   gcc_jit_context_release.  */

recording::context::~context ()
{
  JIT_LOG_SCOPE (get_logger ());
  int i;
  memento *m;
  FOR_EACH_VEC_ELT (m_mementos, i, m)
    {
      delete m;
    }

  for (i = 0; i < GCC_JIT_NUM_STR_OPTIONS; ++i)
    free (m_str_options[i]);

  char *optname;
  FOR_EACH_VEC_ELT (m_command_line_options, i, optname)
    free (optname);
  FOR_EACH_VEC_ELT (m_driver_options, i, optname)
    free (optname);

  if (m_builtins_manager)
    delete m_builtins_manager;

  if (m_owns_first_error_str)
    free (m_first_error_str);

  if (m_owns_last_error_str)
    if (m_last_error_str != m_first_error_str)
      free (m_last_error_str);
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
  JIT_LOG_SCOPE (get_logger ());
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
  JIT_LOG_SCOPE (get_logger ());
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
recording::context::new_string (const char *text, bool escaped)
{
  if (!text)
    return NULL;

  recording::string *result = new string (this, text, escaped);
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
				  int column,
				  bool created_by_user)
{
  recording::location *result =
    new recording::location (this,
			     new_string (filename),
			     line, column,
			     created_by_user);
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
     Compare with tree.cc's make_or_reuse_type.  Note that the _SIZE macros
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
  if (num_bits == 128)
    return get_type (is_signed
		     ? GCC_JIT_TYPE_INT128_T
		     : GCC_JIT_TYPE_UINT128_T);

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

/* Create a recording::bitfield instance and add it to this context's list
   of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_bitfield.  */

recording::field *
recording::context::new_bitfield (recording::location *loc,
				  recording::type *type,
				  int width,
				  const char *name)
{
  recording::field *result =
    new recording::bitfield (this, loc, type, width, new_string (name));
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

/* Create a recording::function_type instance and add it to this context's
   list of mementos.

   Used by new_function_ptr_type and by builtins_manager::make_fn_type.  */

recording::function_type *
recording::context::new_function_type (recording::type *return_type,
				       int num_params,
				       recording::type **param_types,
				       int is_variadic)
{
  recording::function_type *fn_type
    = new function_type (this,
			 return_type,
			 num_params,
			 param_types,
			 is_variadic);
  record (fn_type);
  return fn_type;
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
  recording::function_type *fn_type
    = new_function_type (return_type,
			 num_params,
			 param_types,
			 is_variadic);

  /* Return a pointer-type to the function type.  */
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

/* Locate the builtins_manager (if any) for this family of contexts,
   creating it if it doesn't exist already.

   All of the recording contexts in a family share one builtins_manager:
   if we have a child context, follow the parent links to get the
   ultimate ancestor context, and look for it/store it there.  */

builtins_manager *
recording::context::get_builtins_manager ()
{
  if (m_parent_ctxt)
    return m_parent_ctxt->get_builtins_manager ();

  if (!m_builtins_manager)
    m_builtins_manager = new builtins_manager (this);

  return m_builtins_manager;
}

/* Get a recording::function instance, which is lazily-created and added
   to the context's lists of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_get_builtin_function.  */

recording::function *
recording::context::get_builtin_function (const char *name)
{
  builtins_manager *bm = get_builtins_manager ();
  return bm->get_builtin_function (name);
}

/* Create a recording::global instance and add it to this context's list
   of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_global.  */

recording::lvalue *
recording::context::new_global (recording::location *loc,
				enum gcc_jit_global_kind kind,
				recording::type *type,
				const char *name)
{
  recording::global *result =
    new recording::global (this, loc, kind, type, new_string (name));
  record (result);
  m_globals.safe_push (result);

  return result;
}

void
recording::context::new_global_init_rvalue (lvalue *variable,
					    rvalue *init)
{
  recording::global_init_rvalue *obj =
    new recording::global_init_rvalue (this, variable, init);
  record (obj);

  global *gbl = (global *) variable;
  gbl->set_rvalue_init (init); /* Needed by the global for write dump.  */
}

/* Create a recording::memento_of_typeinfo instance and add it
   to this context's list of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_sizeof.  */

recording::rvalue *
recording::context::new_sizeof (recording::type *type)
{
  recording::rvalue *result =
    new memento_of_typeinfo (this, NULL, type, TYPE_INFO_SIZE_OF);
  record (result);
  return result;
}

/* Create a recording::memento_of_typeinfo instance and add it
   to this context's list of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_alignof.  */

recording::rvalue *
recording::context::new_alignof (recording::type *type)
{
  recording::rvalue *result =
    new memento_of_typeinfo (this, NULL, type, TYPE_INFO_ALIGN_OF);
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

/* Create a recording::memento_of_new_rvalue_from_vector instance and add it
   to this context's list of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_rvalue_from_vector.  */

recording::rvalue *
recording::context::new_rvalue_from_vector (location *loc,
					    vector_type *type,
					    rvalue **elements)
{
  recording::rvalue *result
    = new memento_of_new_rvalue_from_vector (this, loc, type, elements);
  record (result);
  return result;
}

recording::rvalue *
recording::context::new_ctor (recording::location *loc,
			      recording::type *type,
			      size_t num_values,
			      field **fields,
			      rvalue **values)
{
  recording::ctor *result = new ctor (this, loc, type);

  /* Short cut for zero init.  */
  if (!num_values)
    {
      record (result);
      return result;
    }

  bool is_struct_or_union = type->is_struct () || type->is_union ();

  /* We need to copy fields and values into result's auto_vec:s.
     Both for structs and unions and only values for arrays.  */
  if (type->is_array () != NULL)
    {
      result->m_values.reserve (num_values, false);

      for (size_t i = 0; i < num_values; i++)
	result->m_values.quick_push (values[i]);
    }
  else if (is_struct_or_union && fields)
    {
      /* ctor values are paired with user specified fields.  */

      result->m_values.reserve (num_values, false);
      result->m_fields.reserve (num_values, false);

      for (size_t i = 0; i < num_values; i++)
	{
	  result->m_values.quick_push (values[i]);
	  result->m_fields.quick_push (fields[i]);
	}
    }
  else if (is_struct_or_union && !fields)
    {
      /* ctor values are in definition order one by one,
	 so take the fields from the type object.  */

      result->m_values.reserve (num_values, false);
      result->m_fields.reserve (num_values, false);

      compound_type *ct = reinterpret_cast<compound_type *>(type);
      recording::fields *fields = ct->get_fields ();

      /* The entry point checks that num_values is not greater than
	 the amount of fields in 'fields'.  */
      for (size_t i = 0; i < num_values; i++)
	{
	  result->m_values.quick_push (values[i]);
	  result->m_fields.quick_push (fields->get_field (i));
	}
    }
  else
    gcc_unreachable ();

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

/* Create a recording::bitcast instance and add it to this context's list
   of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_bitcast.  */

recording::rvalue *
recording::context::new_bitcast (location *loc,
				 rvalue *expr,
				 type *type_)
{
  recording::rvalue *result = new bitcast (this, loc, expr, type_);
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

/* Create a recording::case_ instance and add it to this context's list
   of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_case.  */

recording::case_ *
recording::context::new_case (recording::rvalue *min_value,
			      recording::rvalue *max_value,
			      recording::block *block)
{
  recording::case_ *result = new case_ (this, min_value, max_value, block);
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
  free (m_str_options[opt]);
  m_str_options[opt] = value ? xstrdup (value) : NULL;
  log_str_option (opt);
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
  log_int_option (opt);
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
  log_bool_option (opt);
}

void
recording::context::set_inner_bool_option (enum inner_bool_option inner_opt,
					   int value)
{
  gcc_assert (inner_opt >= 0 && inner_opt < NUM_INNER_BOOL_OPTIONS);
  m_inner_bool_options[inner_opt] = value ? true : false;
  log_inner_bool_option (inner_opt);
}


/* Add the given optname to this context's list of extra options.

   Implements the post-error-checking part of
   gcc_jit_context_add_command_line_option.  */

void
recording::context::add_command_line_option (const char *optname)
{
  m_command_line_options.safe_push (xstrdup (optname));
}

/* Add any user-provided extra options, starting with any from
   parent contexts.
   Called by playback::context::make_fake_args.  */

void
recording::context::append_command_line_options (vec <char *> *argvec)
{
  if (m_parent_ctxt)
    m_parent_ctxt->append_command_line_options (argvec);

  int i;
  char *optname;
  FOR_EACH_VEC_ELT (m_command_line_options, i, optname)
    argvec->safe_push (xstrdup (optname));
}

/* Add the given optname to this context's list of extra driver options.  */

void
recording::context::add_driver_option (const char *optname)
{
  m_driver_options.safe_push (xstrdup (optname));
}

/* Add any user-provided driver options, starting with any from
   parent contexts.
   Called by playback::context::invoke_driver.  */

void
recording::context::append_driver_options (auto_string_vec *argvec)
{
  if (m_parent_ctxt)
    m_parent_ctxt->append_driver_options (argvec);

  int i;
  char *optname;

  FOR_EACH_VEC_ELT (m_driver_options, i, optname)
    argvec->safe_push (xstrdup (optname));
}

/* Add the given dumpname/out_ptr pair to this context's list of requested
   dumps.

   Implements the post-error-checking part of
   gcc_jit_context_enable_dump.  */

void
recording::context::enable_dump (const char *dumpname,
				 char **out_ptr)
{
  requested_dump d;
  gcc_assert (dumpname);
  gcc_assert (out_ptr);

  d.m_dumpname = dumpname;
  d.m_out_ptr = out_ptr;
  *out_ptr = NULL;
  m_requested_dumps.safe_push (d);
}

/* Validate this context, and if it passes, compile it to memory
   (within a mutex).

   Implements the post-error-checking part of
   gcc_jit_context_compile.  */

result *
recording::context::compile ()
{
  JIT_LOG_SCOPE (get_logger ());

  log_all_options ();

  validate ();

  if (errors_occurred ())
    return NULL;

  /* Set up a compile_to_memory playback context.  */
  ::gcc::jit::playback::compile_to_memory replayer (this);

  /* Use it.  */
  replayer.compile ();

  /* Get the jit::result (or NULL) from the
     compile_to_memory playback context.  */
  return replayer.get_result_obj ();
}

/* Validate this context, and if it passes, compile it to a file
   (within a mutex).

   Implements the post-error-checking part of
   gcc_jit_context_compile_to_file.  */

void
recording::context::compile_to_file (enum gcc_jit_output_kind output_kind,
				     const char *output_path)
{
  JIT_LOG_SCOPE (get_logger ());

  log_all_options ();

  validate ();

  if (errors_occurred ())
    return;

  /* Set up a compile_to_file playback context.  */
  ::gcc::jit::playback::compile_to_file replayer (this,
						  output_kind,
						  output_path);

  /* Use it.  */
  replayer.compile ();
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
  int len;
  char *malloced_msg;
  const char *errmsg;
  bool has_ownership;

  JIT_LOG_SCOPE (get_logger ());

  len = vasprintf (&malloced_msg, fmt, ap);
  if (malloced_msg == NULL || len < 0)
    {
      errmsg = "out of memory generating error message";
      has_ownership = false;
    }
  else
    {
      errmsg = malloced_msg;
      has_ownership = true;
    }
  if (get_logger ())
    get_logger ()->log ("error %i: %s", m_error_count, errmsg);

  const char *ctxt_progname =
    get_str_option (GCC_JIT_STR_OPTION_PROGNAME);
  if (!ctxt_progname)
    ctxt_progname = "libgccjit.so";

  bool print_errors_to_stderr =
      get_inner_bool_option (INNER_BOOL_OPTION_PRINT_ERRORS_TO_STDERR);
  if (print_errors_to_stderr)
  {
    if (loc)
      fprintf (stderr, "%s: %s: error: %s\n",
	       ctxt_progname,
	       loc->get_debug_string (),
	       errmsg);
    else
      fprintf (stderr, "%s: error: %s\n",
	       ctxt_progname,
	       errmsg);
  }

  if (!m_error_count)
    {
      m_first_error_str = const_cast <char *> (errmsg);
      m_owns_first_error_str = has_ownership;
    }

  if (m_owns_last_error_str)
    if (m_last_error_str != m_first_error_str)
      free (m_last_error_str);
  m_last_error_str = const_cast <char *> (errmsg);
  m_owns_last_error_str = has_ownership;

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

/* Get the message for the last error that occurred on this context, or
   NULL if no errors have occurred on it.

   Implements the post-error-checking part of
   gcc_jit_context_get_last_error.  */

const char *
recording::context::get_last_error () const
{
  return m_last_error_str;
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

  /* Globals.  */
  global *g;
  FOR_EACH_VEC_ELT (m_globals, i, g)
    {
      g->write_to_dump (d);
    }
  if (!m_globals.is_empty ())
    d.write ("\n");

  function *fn;
  FOR_EACH_VEC_ELT (m_functions, i, fn)
    {
      fn->write_to_dump (d);
    }

  top_level_asm *tla;
  FOR_EACH_VEC_ELT (m_top_level_asms, i, tla)
    tla->write_to_dump (d);
}

static const char * const
 str_option_reproducer_strings[GCC_JIT_NUM_STR_OPTIONS] = {
  "GCC_JIT_STR_OPTION_PROGNAME"
};

static const char * const
 int_option_reproducer_strings[GCC_JIT_NUM_INT_OPTIONS] = {
  "GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL"
};

static const char * const
 bool_option_reproducer_strings[GCC_JIT_NUM_BOOL_OPTIONS] = {
  "GCC_JIT_BOOL_OPTION_DEBUGINFO",
  "GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE",
  "GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE",
  "GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE",
  "GCC_JIT_BOOL_OPTION_DUMP_SUMMARY",
  "GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING",
  "GCC_JIT_BOOL_OPTION_SELFCHECK_GC",
  "GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES"
};

static const char * const
 inner_bool_option_reproducer_strings[NUM_INNER_BOOL_OPTIONS] = {
  "gcc_jit_context_set_bool_allow_unreachable_blocks",
  "gcc_jit_context_set_bool_use_external_driver",
  "gcc_jit_context_set_bool_print_errors_to_stderr",
};

/* Write the current value of all options to the log file (if any).  */

void
recording::context::log_all_options () const
{
  int opt_idx;

  if (!get_logger ())
    return;

  for (opt_idx = 0; opt_idx < GCC_JIT_NUM_STR_OPTIONS; opt_idx++)
    log_str_option ((enum gcc_jit_str_option)opt_idx);

  for (opt_idx = 0; opt_idx < GCC_JIT_NUM_INT_OPTIONS; opt_idx++)
    log_int_option ((enum gcc_jit_int_option)opt_idx);

  for (opt_idx = 0; opt_idx < GCC_JIT_NUM_BOOL_OPTIONS; opt_idx++)
    log_bool_option ((enum gcc_jit_bool_option)opt_idx);
  for (opt_idx = 0; opt_idx < NUM_INNER_BOOL_OPTIONS; opt_idx++)
    log_inner_bool_option ((enum inner_bool_option)opt_idx);
}

/* Write the current value of the given string option to the
   log file (if any).  */

void
recording::context::log_str_option (enum gcc_jit_str_option opt) const
{
  gcc_assert (opt < GCC_JIT_NUM_STR_OPTIONS);
  if (get_logger ())
    {
      if (m_str_options[opt])
	log ("%s: \"%s\"",
	     str_option_reproducer_strings[opt],
	     m_str_options[opt]);
      else
	log ("%s: NULL",
	     str_option_reproducer_strings[opt]);
    }
}

/* Write the current value of the given int option to the
   log file (if any).  */

void
recording::context::log_int_option (enum gcc_jit_int_option opt) const
{
  gcc_assert (opt < GCC_JIT_NUM_INT_OPTIONS);
  if (get_logger ())
    log ("%s: %i",
	 int_option_reproducer_strings[opt],
	 m_int_options[opt]);
}

/* Write the current value of the given bool option to the
   log file (if any).  */

void
recording::context::log_bool_option (enum gcc_jit_bool_option opt) const
{
  gcc_assert (opt < GCC_JIT_NUM_BOOL_OPTIONS);
  if (get_logger ())
    log ("%s: %s",
	 bool_option_reproducer_strings[opt],
	 m_bool_options[opt] ? "true" : "false");
}

/* Write the current value of the given "inner" bool option to the
   log file (if any).  */

void
recording::context::log_inner_bool_option (enum inner_bool_option opt) const
{
  gcc_assert (opt < NUM_INNER_BOOL_OPTIONS);
  if (get_logger ())
    log ("%s: %s",
	 inner_bool_option_reproducer_strings[opt],
	 m_inner_bool_options[opt] ? "true" : "false");
}

/* Write C source code to PATH that attempts to replay the API
   calls made to this context (and its parents), for use in
   minimizing test cases for libgccjit.

   Implements the post-error-checking part of
   gcc_jit_context_dump_reproducer_to_file.  */

void
recording::context::dump_reproducer_to_file (const char *path)
{
  JIT_LOG_SCOPE (get_logger ());
  reproducer r (*this, path);

  /* Generate the "ancestry" of this context, as a list.  */
  auto_vec <context *> ascending_contexts;
  for (context *ctxt = this; ctxt; ctxt = ctxt->m_parent_ctxt)
    ascending_contexts.safe_push (ctxt);

  /* Reverse the list, giving a list of contexts from
     top-most parent context down through to youngest child context.
     We will use this list as the parameters of the functions in
     our generated file.  */
  unsigned num_ctxts = ascending_contexts.length ();
  auto_vec <context *> contexts (num_ctxts);
  for (unsigned i = 0; i < num_ctxts; i++)
    contexts.safe_push (ascending_contexts[num_ctxts - (i + 1)]);

  /* contexts[0] should be the top-level context.  */
  gcc_assert (contexts[0]);
  gcc_assert (contexts[0]->m_toplevel_ctxt == contexts[0]);

  /* The final element in contexts should be "this".  */
  gcc_assert (contexts[contexts.length () - 1] == this);
  gcc_assert (contexts[contexts.length () - 1]->m_toplevel_ctxt
	      == contexts[0]);

  r.write ("/* This code was autogenerated by"
	   " gcc_jit_context_dump_reproducer_to_file.\n\n");
  print_version (r.get_file (), "  ", false);
  r.write ("*/\n");
  r.write ("#include <libgccjit.h>\n\n");
  r.write ("#pragma GCC diagnostic ignored \"-Wunused-variable\"\n\n");
  r.write ("static void\nset_options (");
  r.write_params (contexts);
  r.write (");\n\n");
  r.write ("static void\ncreate_code (");
  r.write_params (contexts);
  r.write (");\n\n");
  r.write ("int\nmain (int argc, const char **argv)\n");
  r.write ("{\n");
  for (unsigned i = 0; i < num_ctxts; i++)
    r.write ("  gcc_jit_context *%s;\n",
	     r.get_identifier (contexts[i]));
  r.write ("  gcc_jit_result *result;\n"
	   "\n");

  /* Create the contexts.
     The top-level context is acquired from a clean slate, the others as
     children of the prior context.  */
  r.write ("  %s = gcc_jit_context_acquire ();\n",
	   r.get_identifier (contexts[0]));
  for (unsigned i = 1; i < num_ctxts; i++)
    r.write ("  %s = gcc_jit_context_new_child_context (%s);\n",
	     r.get_identifier (contexts[i]),
	     r.get_identifier (contexts[i - 1]));
  r.write ("  set_options (");
  r.write_args (contexts);
  r.write (");\n");
  r.write ("  create_code (");
  r.write_args (contexts);
  r.write (");\n");

  r.write ("  result = gcc_jit_context_compile (%s);\n",
	   r.get_identifier (this));

  for (unsigned i = num_ctxts; i > 0; i--)
    r.write ("  gcc_jit_context_release (%s);\n",
	     r.get_identifier (contexts[i - 1]));

  r.write ("  gcc_jit_result_release (result);\n"
	   "  return 0;\n"
	   "}\n\n");

  /* Define (char *) variables for use in calls to
     gcc_jit_context_enable_dump.  */
  for (unsigned ctxt_idx = 0; ctxt_idx < num_ctxts; ctxt_idx++)
    {
      if (m_requested_dumps.length ())
	{
	  r.write ("/* Requested dumps for %s.  */\n",
		   r.get_identifier (contexts[ctxt_idx]));
	  for (unsigned i = 0; i < m_requested_dumps.length (); i++)
	    r.write ("static char *dump_%p;\n",
		     (void *)&m_requested_dumps[i]);
	  r.write ("\n");
	}
    }

  /* Write out values of options.  */
  r.write ("static void\nset_options (");
  r.write_params (contexts);
  r.write (")\n{\n");
  for (unsigned ctxt_idx = 0; ctxt_idx < num_ctxts; ctxt_idx++)
    {
      if (ctxt_idx > 0)
	r.write ("\n");

      r.write ("  /* Set options for %s.  */\n",
	       r.get_identifier (contexts[ctxt_idx]));

      r.write ("  /* String options.  */\n");
      for (int opt_idx = 0; opt_idx < GCC_JIT_NUM_STR_OPTIONS; opt_idx++)
	{
	  r.write ("  gcc_jit_context_set_str_option (%s,\n"
		   "                                  %s,\n",
		   r.get_identifier (contexts[ctxt_idx]),
		   str_option_reproducer_strings[opt_idx]);
	  if (m_str_options[opt_idx])
	    r.write ("                                  \"%s\");\n",
		     m_str_options[opt_idx]);
	  else
	    r.write ("                                  NULL);\n");
	}
      r.write ("  /* Int options.  */\n");
      for (int opt_idx = 0; opt_idx < GCC_JIT_NUM_INT_OPTIONS; opt_idx++)
	r.write ("  gcc_jit_context_set_int_option (%s,\n"
		 "                                  %s,\n"
		 "                                  %i);\n",
		 r.get_identifier (contexts[ctxt_idx]),
		 int_option_reproducer_strings[opt_idx],
		 m_int_options[opt_idx]);
      r.write ("  /* Boolean options.  */\n");
      for (int opt_idx = 0; opt_idx < GCC_JIT_NUM_BOOL_OPTIONS; opt_idx++)
	r.write ("  gcc_jit_context_set_bool_option (%s,\n"
		 "                                  %s,\n"
		 "                                  %i);\n",
		 r.get_identifier (contexts[ctxt_idx]),
		 bool_option_reproducer_strings[opt_idx],
		 m_bool_options[opt_idx]);
      for (int opt_idx = 0; opt_idx < NUM_INNER_BOOL_OPTIONS; opt_idx++)
	r.write ("  %s (%s, %i);\n",
		 inner_bool_option_reproducer_strings[opt_idx],
		 r.get_identifier (contexts[ctxt_idx]),
		 m_inner_bool_options[opt_idx]);

      if (!m_command_line_options.is_empty ())
	{
	  int i;
	  char *optname;
	  r.write ("  /* User-provided command-line options.  */\n");
	  FOR_EACH_VEC_ELT (m_command_line_options, i, optname)
	    r.write ("  gcc_jit_context_add_command_line_option (%s, \"%s\");\n",
		     r.get_identifier (contexts[ctxt_idx]),
		     optname);
	}

      if (!m_driver_options.is_empty ())
	{
	  int i;
	  char *optname;
	  r.write ("  /* User-provided driver options.  */\n");
	  FOR_EACH_VEC_ELT (m_driver_options, i, optname)
	    r.write ("  gcc_jit_context_add_driver_option (%s, \"%s\");\n",
		     r.get_identifier (contexts[ctxt_idx]),
		     optname);
	}

      if (m_requested_dumps.length ())
	{
	  r.write ("  /* Requested dumps.  */\n");
	  /* Dumpfiles that were requested via gcc_jit_context_enable_dump.  */
	  for (unsigned i = 0; i < m_requested_dumps.length (); i++)
	    {
	      r.write ("  gcc_jit_context_enable_dump (%s,\n"
		       "                               \"%s\",\n"
		       "                               &dump_%p);\n",
		       r.get_identifier (contexts[ctxt_idx]),
		       m_requested_dumps[i].m_dumpname,
		       (void *)&m_requested_dumps[i]);
	    }
	}
    }
  r.write ("}\n\n");

  r.write ("static void\ncreate_code (");
  r.write_params (contexts);
  r.write (")\n"
	   "{\n");
  for (unsigned ctxt_idx = 0; ctxt_idx < num_ctxts; ctxt_idx++)
    {
      memento *m;
      int i;
      if (ctxt_idx > 0)
	r.write ("\n\n");

      r.write ("  /* Replay of API calls for %s.  */\n",
	       r.get_identifier (contexts[ctxt_idx]));
      FOR_EACH_VEC_ELT (contexts[ctxt_idx]->m_mementos, i, m)
	m->write_reproducer (r);
    }
  r.write ("}\n");
}

/* Copy the requested dumps within this context and all ancestors into
   OUT. */

void
recording::context::get_all_requested_dumps (vec <recording::requested_dump> *out)
{
  if (m_parent_ctxt)
    m_parent_ctxt->get_all_requested_dumps (out);

  out->reserve (m_requested_dumps.length ());
  out->splice (m_requested_dumps);
}

/* Create a recording::top_level_asm instance and add it to this
   context's list of mementos and to m_top_level_asms.

   Implements the post-error-checking part of
   gcc_jit_context_add_top_level_asm.  */

void
recording::context::add_top_level_asm (recording::location *loc,
				       const char *asm_stmts)
{
  recording::top_level_asm *asm_obj
    = new recording::top_level_asm (this, loc, new_string (asm_stmts));
  record (asm_obj);
  m_top_level_asms.safe_push (asm_obj);
}

/* This is a pre-compilation check for the context (and any parents).

   Detect errors within the context, adding errors if any are found.  */

void
recording::context::validate ()
{
  JIT_LOG_SCOPE (get_logger ());

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
  d.write ("  %s\n", get_debug_string ());
}

/* The implementation of class gcc::jit::recording::string.  */

/* Constructor for gcc::jit::recording::string::string, allocating a
   copy of the given text using new char[].  */

recording::string::string (context *ctxt, const char *text, bool escaped)
: memento (ctxt),
  m_escaped (escaped)
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
  int len;
  va_list ap;
  char *buf;
  recording::string *result;

  va_start (ap, fmt);
  len = vasprintf (&buf, fmt, ap);
  va_end (ap);

  if (buf == NULL || len < 0)
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
  /* Avoid infinite recursion into strings when logging all mementos:
     don't re-escape strings:  */
  if (m_escaped)
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
      switch (ch)
	{
	default:
	  APPEND(ch);
	  break;
	case '\t':
	  APPEND('\\');
	  APPEND('t');
	  break;
	case '\n':
	  APPEND('\\');
	  APPEND('n');
	  break;
	case '\\':
	case '"':
	  APPEND('\\');
	  APPEND(ch);
	  break;
	}
    }
  APPEND('"'); /* closing quote */
#undef APPEND
  tmp[len] = '\0'; /* nil termintator */

  string *result = m_ctxt->new_string (tmp, true);

  delete[] tmp;
  return result;
}

/* Implementation of recording::memento::write_reproducer for strings. */

void
recording::string::write_reproducer (reproducer &)
{
  /* Empty.  */
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

/* Implementation of recording::memento::write_reproducer for locations. */

void
recording::location::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "loc");
  r.write ("  gcc_jit_location *%s =\n"
	   "    gcc_jit_context_new_location (%s, /* gcc_jit_context *ctxt */\n"
	   "    %s, /* const char *filename */\n"
	   "    %i, /* int line */\n"
	   "    %i);/* int column */\n",
	   id,
	   r.get_identifier (get_context ()),
	   m_filename->get_debug_string (),
	   m_line, m_column);
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

/* Given a type T, get the type restrict T.

   Implements the post-error-checking part of
   gcc_jit_type_get_restrict.  */

recording::type *
recording::type::get_restrict ()
{
  recording::type *result = new memento_of_get_restrict (this);
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

/* Given a type, get an aligned version of the type.

   Implements the post-error-checking part of
   gcc_jit_type_get_aligned.  */

recording::type *
recording::type::get_aligned (size_t alignment_in_bytes)
{
  recording::type *result
    = new memento_of_get_aligned (this, alignment_in_bytes);
  m_ctxt->record (result);
  return result;
}

/* Given a type, get a vector version of the type.

   Implements the post-error-checking part of
   gcc_jit_type_get_vector.  */

recording::type *
recording::type::get_vector (size_t num_units)
{
  recording::type *result
    = new vector_type (this, num_units);
  m_ctxt->record (result);
  return result;
}

const char *
recording::type::access_as_type (reproducer &r)
{
  return r.get_identifier (this);
}

/* Override of default implementation of
   recording::type::get_size.

   Return the size in bytes.  This is in use for global
   initialization.  */

size_t
recording::memento_of_get_type::get_size ()
{
  int size;
  machine_mode m;
  switch (m_kind)
    {
    case GCC_JIT_TYPE_VOID:
      return 0;
    case GCC_JIT_TYPE_BOOL:
    case GCC_JIT_TYPE_CHAR:
    case GCC_JIT_TYPE_SIGNED_CHAR:
    case GCC_JIT_TYPE_UNSIGNED_CHAR:
      return 1;
    case GCC_JIT_TYPE_SHORT:
    case GCC_JIT_TYPE_UNSIGNED_SHORT:
      size = SHORT_TYPE_SIZE;
      break;
    case GCC_JIT_TYPE_INT:
    case GCC_JIT_TYPE_UNSIGNED_INT:
      size = INT_TYPE_SIZE;
      break;
    case GCC_JIT_TYPE_LONG:
    case GCC_JIT_TYPE_UNSIGNED_LONG:
      size = LONG_TYPE_SIZE;
      break;
    case GCC_JIT_TYPE_LONG_LONG:
    case GCC_JIT_TYPE_UNSIGNED_LONG_LONG:
      size = LONG_LONG_TYPE_SIZE;
      break;
    case GCC_JIT_TYPE_UINT8_T:
    case GCC_JIT_TYPE_INT8_T:
      size = 8;
      break;
    case GCC_JIT_TYPE_UINT16_T:
    case GCC_JIT_TYPE_INT16_T:
      size = 16;
      break;
    case GCC_JIT_TYPE_UINT32_T:
    case GCC_JIT_TYPE_INT32_T:
      size = 32;
      break;
    case GCC_JIT_TYPE_UINT64_T:
    case GCC_JIT_TYPE_INT64_T:
      size = 64;
      break;
    case GCC_JIT_TYPE_UINT128_T:
    case GCC_JIT_TYPE_INT128_T:
      size = 128;
      break;
    case GCC_JIT_TYPE_FLOAT:
      m = targetm.c.mode_for_floating_type (TI_FLOAT_TYPE);
      size = GET_MODE_PRECISION (m).to_constant ();
      break;
#ifdef HAVE_BFmode
    case GCC_JIT_TYPE_BFLOAT16:
      return GET_MODE_UNIT_SIZE (BFmode);
#endif
    case GCC_JIT_TYPE_DOUBLE:
      m = targetm.c.mode_for_floating_type (TI_DOUBLE_TYPE);
      size = GET_MODE_PRECISION (m).to_constant ();
      break;
    case GCC_JIT_TYPE_LONG_DOUBLE:
      m = targetm.c.mode_for_floating_type (TI_LONG_DOUBLE_TYPE);
      size = GET_MODE_PRECISION (m).to_constant ();
      break;
    case GCC_JIT_TYPE_SIZE_T:
      size = MAX_BITS_PER_WORD;
      break;
    default:
      /* As this function is called by
	 'gcc_jit_global_set_initializer' and
	 'recording::global::write_reproducer' possible types are only
	 integrals and are covered by the previous cases.  */
      gcc_unreachable ();
    }

  return size / BITS_PER_UNIT;
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
    case GCC_JIT_TYPE_UINT8_T:
    case GCC_JIT_TYPE_UINT16_T:
    case GCC_JIT_TYPE_UINT32_T:
    case GCC_JIT_TYPE_UINT64_T:
    case GCC_JIT_TYPE_UINT128_T:
    case GCC_JIT_TYPE_INT8_T:
    case GCC_JIT_TYPE_INT16_T:
    case GCC_JIT_TYPE_INT32_T:
    case GCC_JIT_TYPE_INT64_T:
    case GCC_JIT_TYPE_INT128_T:
    case GCC_JIT_TYPE_FLOAT:
    case GCC_JIT_TYPE_BFLOAT16:
    case GCC_JIT_TYPE_DOUBLE:
    case GCC_JIT_TYPE_LONG_DOUBLE:
    case GCC_JIT_TYPE_COMPLEX_FLOAT:
    case GCC_JIT_TYPE_COMPLEX_DOUBLE:
    case GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE:
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
    case GCC_JIT_TYPE_UINT8_T:
    case GCC_JIT_TYPE_UINT16_T:
    case GCC_JIT_TYPE_UINT32_T:
    case GCC_JIT_TYPE_UINT64_T:
    case GCC_JIT_TYPE_UINT128_T:
    case GCC_JIT_TYPE_INT8_T:
    case GCC_JIT_TYPE_INT16_T:
    case GCC_JIT_TYPE_INT32_T:
    case GCC_JIT_TYPE_INT64_T:
    case GCC_JIT_TYPE_INT128_T:
      return true;

    case GCC_JIT_TYPE_FLOAT:
    case GCC_JIT_TYPE_BFLOAT16:
    case GCC_JIT_TYPE_DOUBLE:
    case GCC_JIT_TYPE_LONG_DOUBLE:
      return false;

    case GCC_JIT_TYPE_CONST_CHAR_PTR:
      return false;

    case GCC_JIT_TYPE_SIZE_T:
      return true;

    case GCC_JIT_TYPE_FILE_PTR:
      return false;

    case GCC_JIT_TYPE_COMPLEX_FLOAT:
    case GCC_JIT_TYPE_COMPLEX_DOUBLE:
    case GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE:
      return false;
    }
}

/* Implementation of pure virtual hook recording::type::is_signed for
   recording::memento_of_get_type.  */

bool
recording::memento_of_get_type::is_signed () const
{
  switch (m_kind)
    {
    default: gcc_unreachable ();

    case GCC_JIT_TYPE_SIGNED_CHAR:
    case GCC_JIT_TYPE_CHAR:
    case GCC_JIT_TYPE_SHORT:
    case GCC_JIT_TYPE_INT:
    case GCC_JIT_TYPE_LONG:
    case GCC_JIT_TYPE_LONG_LONG:
    case GCC_JIT_TYPE_INT8_T:
    case GCC_JIT_TYPE_INT16_T:
    case GCC_JIT_TYPE_INT32_T:
    case GCC_JIT_TYPE_INT64_T:
    case GCC_JIT_TYPE_INT128_T:
      return true;

    case GCC_JIT_TYPE_VOID:
    case GCC_JIT_TYPE_VOID_PTR:
    case GCC_JIT_TYPE_BOOL:
    case GCC_JIT_TYPE_UNSIGNED_CHAR:
    case GCC_JIT_TYPE_UNSIGNED_SHORT:
    case GCC_JIT_TYPE_UNSIGNED_INT:
    case GCC_JIT_TYPE_UNSIGNED_LONG:
    case GCC_JIT_TYPE_UNSIGNED_LONG_LONG:
    case GCC_JIT_TYPE_UINT8_T:
    case GCC_JIT_TYPE_UINT16_T:
    case GCC_JIT_TYPE_UINT32_T:
    case GCC_JIT_TYPE_UINT64_T:
    case GCC_JIT_TYPE_UINT128_T:

    case GCC_JIT_TYPE_FLOAT:
    case GCC_JIT_TYPE_BFLOAT16:
    case GCC_JIT_TYPE_DOUBLE:
    case GCC_JIT_TYPE_LONG_DOUBLE:

    case GCC_JIT_TYPE_CONST_CHAR_PTR:

    case GCC_JIT_TYPE_SIZE_T:

    case GCC_JIT_TYPE_FILE_PTR:

    case GCC_JIT_TYPE_COMPLEX_FLOAT:
    case GCC_JIT_TYPE_COMPLEX_DOUBLE:
    case GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE:
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
    case GCC_JIT_TYPE_UINT8_T:
    case GCC_JIT_TYPE_UINT16_T:
    case GCC_JIT_TYPE_UINT32_T:
    case GCC_JIT_TYPE_UINT64_T:
    case GCC_JIT_TYPE_UINT128_T:
    case GCC_JIT_TYPE_INT8_T:
    case GCC_JIT_TYPE_INT16_T:
    case GCC_JIT_TYPE_INT32_T:
    case GCC_JIT_TYPE_INT64_T:
    case GCC_JIT_TYPE_INT128_T:
      return false;

    case GCC_JIT_TYPE_FLOAT:
    case GCC_JIT_TYPE_BFLOAT16:
    case GCC_JIT_TYPE_DOUBLE:
    case GCC_JIT_TYPE_LONG_DOUBLE:
      return true;

    case GCC_JIT_TYPE_CONST_CHAR_PTR:
      return false;

    case GCC_JIT_TYPE_SIZE_T:
      return false;

    case GCC_JIT_TYPE_FILE_PTR:
      return false;

    case GCC_JIT_TYPE_COMPLEX_FLOAT:
    case GCC_JIT_TYPE_COMPLEX_DOUBLE:
    case GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE:
      return true;
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
    case GCC_JIT_TYPE_UINT8_T:
    case GCC_JIT_TYPE_UINT16_T:
    case GCC_JIT_TYPE_UINT32_T:
    case GCC_JIT_TYPE_UINT64_T:
    case GCC_JIT_TYPE_UINT128_T:
    case GCC_JIT_TYPE_INT8_T:
    case GCC_JIT_TYPE_INT16_T:
    case GCC_JIT_TYPE_INT32_T:
    case GCC_JIT_TYPE_INT64_T:
    case GCC_JIT_TYPE_INT128_T:
      return false;

    case GCC_JIT_TYPE_FLOAT:
    case GCC_JIT_TYPE_BFLOAT16:
    case GCC_JIT_TYPE_DOUBLE:
    case GCC_JIT_TYPE_LONG_DOUBLE:
      return false;

    case GCC_JIT_TYPE_CONST_CHAR_PTR:
      return false;

    case GCC_JIT_TYPE_SIZE_T:
      return false;

    case GCC_JIT_TYPE_FILE_PTR:
      return false;

    case GCC_JIT_TYPE_COMPLEX_FLOAT:
    case GCC_JIT_TYPE_COMPLEX_DOUBLE:
    case GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE:
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

  "FILE *",  /* GCC_JIT_TYPE_FILE_PTR */

  "complex float", /* GCC_JIT_TYPE_COMPLEX_FLOAT */
  "complex double", /* GCC_JIT_TYPE_COMPLEX_DOUBLE */
  "complex long double",  /* GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE */

  "__uint8_t",    /* GCC_JIT_TYPE_UINT8_T */
  "__uint16_t",   /* GCC_JIT_TYPE_UINT16_T */
  "__uint32_t",   /* GCC_JIT_TYPE_UINT32_T */
  "__uint64_t",   /* GCC_JIT_TYPE_UINT64_T */
  "__uint128_t",  /* GCC_JIT_TYPE_UINT128_T */
  "__int8_t",     /* GCC_JIT_TYPE_INT8_T */
  "__int16_t",    /* GCC_JIT_TYPE_INT16_T */
  "__int32_t",    /* GCC_JIT_TYPE_INT32_T */
  "__int64_t",    /* GCC_JIT_TYPE_INT64_T */
  "__int128_t",   /* GCC_JIT_TYPE_INT128_T */

  "bfloat16", /* GCC_JIT_TYPE_BFLOAT16 */
};

/* Implementation of recording::memento::make_debug_string for
   results of get_type, using a simple table of type names.  */

recording::string *
recording::memento_of_get_type::make_debug_string ()
{
  return m_ctxt->new_string (get_type_strings[m_kind]);
}

static const char * const get_type_enum_strings[] = {
  "GCC_JIT_TYPE_VOID",
  "GCC_JIT_TYPE_VOID_PTR",
  "GCC_JIT_TYPE_BOOL",
  "GCC_JIT_TYPE_CHAR",
  "GCC_JIT_TYPE_SIGNED_CHAR",
  "GCC_JIT_TYPE_UNSIGNED_CHAR",
  "GCC_JIT_TYPE_SHORT",
  "GCC_JIT_TYPE_UNSIGNED_SHORT",
  "GCC_JIT_TYPE_INT",
  "GCC_JIT_TYPE_UNSIGNED_INT",
  "GCC_JIT_TYPE_LONG",
  "GCC_JIT_TYPE_UNSIGNED_LONG",
  "GCC_JIT_TYPE_LONG_LONG",
  "GCC_JIT_TYPE_UNSIGNED_LONG_LONG",
  "GCC_JIT_TYPE_FLOAT",
  "GCC_JIT_TYPE_DOUBLE",
  "GCC_JIT_TYPE_LONG_DOUBLE",
  "GCC_JIT_TYPE_CONST_CHAR_PTR",
  "GCC_JIT_TYPE_SIZE_T",
  "GCC_JIT_TYPE_FILE_PTR",
  "GCC_JIT_TYPE_COMPLEX_FLOAT",
  "GCC_JIT_TYPE_COMPLEX_DOUBLE",
  "GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE",
  "GCC_JIT_TYPE_UINT8_T",
  "GCC_JIT_TYPE_UINT16_T",
  "GCC_JIT_TYPE_UINT32_T",
  "GCC_JIT_TYPE_UINT64_T",
  "GCC_JIT_TYPE_UINT128_T",
  "GCC_JIT_TYPE_INT8_T",
  "GCC_JIT_TYPE_INT16_T",
  "GCC_JIT_TYPE_INT32_T",
  "GCC_JIT_TYPE_INT64_T",
  "GCC_JIT_TYPE_INT128_T",
  "GCC_JIT_TYPE_BFLOAT16",
};

void
recording::memento_of_get_type::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "type");
  r.write ("  gcc_jit_type *%s = gcc_jit_context_get_type (%s, %s);\n",
	   id,
	   r.get_identifier (get_context ()),
	   get_type_enum_strings[m_kind]);
}

/* The implementation of class gcc::jit::recording::memento_of_get_pointer.  */

/* Override of default implementation of
   recording::type::get_size for get_pointer.  */

size_t
recording::memento_of_get_pointer::get_size ()
{
  return POINTER_SIZE / BITS_PER_UNIT;
}

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
  if (m_other_type->unqualified ()->accepts_writes_from (rtype_points_to))
  {
    return true;
  }

  /* It's OK to assign to a (volatile const T *) from a (volatile const T *). */
  return m_other_type->is_same_type_as (rtype_points_to);
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

/* Implementation of recording::memento::write_reproducer for get_pointer.  */

void
recording::memento_of_get_pointer::write_reproducer (reproducer &r)
{
  /* We need to special-case function pointer types; see the notes in
     recording::function_type::write_deferred_reproducer.  */
  if (function_type *fn_type = m_other_type->dyn_cast_function_type ())
    {
      fn_type->write_deferred_reproducer (r, this);
      return;
    }

  const char *id = r.make_identifier (this, "type");
  r.write ("  gcc_jit_type *%s =\n"
	   "    gcc_jit_type_get_pointer (%s);\n",
	   id,
	   r.get_identifier_as_type (m_other_type));
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

/* Implementation of recording::memento::write_reproducer for const types. */

void
recording::memento_of_get_const::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "type");
  r.write ("  gcc_jit_type *%s =\n"
	   "    gcc_jit_type_get_const (%s);\n",
	   id,
	   r.get_identifier_as_type (m_other_type));
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

/* Implementation of recording::memento::write_reproducer for volatile
   types. */

void
recording::memento_of_get_volatile::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "type");
  r.write ("  gcc_jit_type *%s =\n"
	   "    gcc_jit_type_get_volatile (%s);\n",
	   id,
	   r.get_identifier_as_type (m_other_type));
}

/* The implementation of class gcc::jit::recording::memento_of_get_restrict.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::memento_of_get_restrict.  */

void
recording::memento_of_get_restrict::replay_into (replayer *)
{
  set_playback_obj (m_other_type->playback_type ()->get_restrict ());
}

/* Implementation of recording::memento::make_debug_string for
   results of get_restrict, prepending "restrict ".  */

recording::string *
recording::memento_of_get_restrict::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "restrict %s", m_other_type->get_debug_string ());
}

/* Implementation of recording::memento::write_reproducer for restrict
   types.  */

void
recording::memento_of_get_restrict::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "type");
  r.write ("  gcc_jit_type *%s =\n"
	   "    gcc_jit_type_get_restrict (%s);\n",
	   id,
	   r.get_identifier_as_type (m_other_type));
}

/* The implementation of class gcc::jit::recording::memento_of_get_aligned.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::memento_of_get_aligned.  */

void
recording::memento_of_get_aligned::replay_into (replayer *)
{
  set_playback_obj
    (m_other_type->playback_type ()->get_aligned (m_alignment_in_bytes));
}

/* Implementation of recording::memento::make_debug_string for
   results of get_aligned.  */

recording::string *
recording::memento_of_get_aligned::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "%s  __attribute__((aligned(%zi)))",
			      m_other_type->get_debug_string (),
			      m_alignment_in_bytes);
}

/* Implementation of recording::memento::write_reproducer for aligned
   types. */

void
recording::memento_of_get_aligned::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "type");
  r.write ("  gcc_jit_type *%s =\n"
	   "    gcc_jit_type_get_aligned (%s, %zi);\n",
	   id,
	   r.get_identifier_as_type (m_other_type),
	   m_alignment_in_bytes);
}

/* The implementation of class gcc::jit::recording::vector_type.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::vector_type.  */

void
recording::vector_type::replay_into (replayer *)
{
  set_playback_obj
    (m_other_type->playback_type ()->get_vector (m_num_units));
}

/* Implementation of recording::memento::make_debug_string for
   results of get_vector.  */

recording::string *
recording::vector_type::make_debug_string ()
{
  return string::from_printf
    (m_ctxt,
     "%s  __attribute__((vector_size(sizeof (%s) * %zi)))",
     m_other_type->get_debug_string (),
     m_other_type->get_debug_string (),
     m_num_units);
}

/* Implementation of recording::memento::write_reproducer for vector types. */

void
recording::vector_type::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "type");
  r.write ("  gcc_jit_type *%s =\n"
	   "    gcc_jit_type_get_vector (%s, %zi);\n",
	   id,
	   r.get_identifier_as_type (m_other_type),
	   m_num_units);
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

/* Implementation of recording::memento::write_reproducer for array
   types. */

void
recording::array_type::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "array_type");
  r.write ("  gcc_jit_type *%s =\n"
	   "    gcc_jit_context_new_array_type (%s,\n"
	   "                                    %s, /* gcc_jit_location *loc */\n"
	   "                                    %s, /* gcc_jit_type *element_type */\n"
	   "                                    %i); /* int num_elements */\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   r.get_identifier_as_type (m_element_type),
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

/* Implementation of virtual hook recording::type::is_same_type_as for
   recording::function_type.

   We override this to avoid requiring identity of function pointer types,
   so that if client code has obtained the same signature in
   different ways (e.g. via gcc_jit_context_new_function_ptr_type
   vs gcc_jit_function_get_address), the different function_type
   instances are treated as compatible.

   We can't use type::accepts_writes_from for this as we need a stronger
   notion of "sameness": if we have a fn_ptr type that has args that are
   themselves fn_ptr types, then those args still need to match exactly.

   Alternatively, we could consolidate attempts to create identical
   function_type instances so that pointer equality works, but that runs
   into issues about the lifetimes of the cache (w.r.t. nested contexts).  */

bool
recording::function_type::is_same_type_as (type *other)
{
  gcc_assert (other);

  function_type *other_fn_type = other->dyn_cast_function_type ();
  if (!other_fn_type)
    return false;

  /* Everything must match.  */

  if (!m_return_type->is_same_type_as (other_fn_type->m_return_type))
    return false;

  if (m_param_types.length () != other_fn_type->m_param_types.length ())
    return false;

  unsigned i;
  type *param_type;
  FOR_EACH_VEC_ELT (m_param_types, i, param_type)
    if (!param_type->is_same_type_as (other_fn_type->m_param_types[i]))
      return false;

  if (m_is_variadic != other_fn_type->m_is_variadic)
    return false;

  /* Passed all tests.  */
  return true;
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::function_type.  */

void
recording::function_type::replay_into (replayer *r)
{
  /* Convert m_param_types to a vec of playback type.  */
  auto_vec <playback::type *> param_types;
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

/* Implementation of recording::memento::write_reproducer for function
   types.  */

void
recording::function_type::write_reproducer (reproducer &)
{
  /* see notes below.  */
}

/* There's a get_pointer within context::new_function_ptr_type:
   the type received by client code isn't the memento for the
   function_type, but instead the result of get_pointer on it.

   Hence we can't directly write a reproducer that gives function_type.
   Instead we special-case things within get_pointer, detecting this
   case, calling the following function.  */

void
recording::function_type::write_deferred_reproducer (reproducer &r,
						     memento *ptr_type)
{
  gcc_assert (ptr_type);
  r.make_identifier (this, "function_type");
  const char *ptr_id = r.make_identifier (ptr_type, "ptr_to");
  const char *param_types_id = r.make_tmp_identifier ("params_for", this);
  r.write ("  gcc_jit_type *%s[%i] = {\n",
	   param_types_id,
	   m_param_types.length ());
  int i;
  type *param_type;
  FOR_EACH_VEC_ELT (m_param_types, i, param_type)
    r.write ("    %s,\n", r.get_identifier_as_type (param_type));
  r.write ("  };\n");
  r.write ("  gcc_jit_type *%s =\n"
	   "    gcc_jit_context_new_function_ptr_type (%s, /* gcc_jit_context *ctxt */\n"
	   "                                           %s, /* gcc_jit_location *loc */\n"
	   "                                           %s, /* gcc_jit_type *return_type */\n"
	   "                                           %i, /* int num_params */\n"
	   "                                           %s, /* gcc_jit_type **param_types */\n"
	   "                                           %i); /* int is_variadic */\n",
	   ptr_id,
	   r.get_identifier (get_context ()),
	   "NULL", /* location is not stored */
	   r.get_identifier_as_type (m_return_type),
	   m_param_types.length (),
	   param_types_id,
	   m_is_variadic);
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
   so that we can build up a struct/union field by field.  */

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

/* Implementation of recording::memento::write_reproducer for fields.  */

void
recording::field::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "field");
  r.write("  gcc_jit_field *%s =\n"
	  "    gcc_jit_context_new_field (%s,\n"
	  "                               %s, /* gcc_jit_location *loc */\n"
	  "                               %s, /* gcc_jit_type *type, */\n"
	  "                               %s); /* const char *name */\n",
	  id,
	  r.get_identifier (get_context ()),
	  r.get_identifier (m_loc),
	  r.get_identifier_as_type (m_type),
	  m_name->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::bitfield.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::bitfield.  */

void
recording::bitfield::replay_into (replayer *r)
{
  set_playback_obj (r->new_bitfield (playback_location (r, m_loc),
				     m_type->playback_type (),
				     m_width,
				     playback_string (m_name)));
}

/* Override the default implementation of
   recording::memento::write_to_dump.  Dump each bit field
   by dumping a line of the form:
      TYPE NAME:WIDTH;
   so that we can build up a struct/union field by field.  */

void
recording::bitfield::write_to_dump (dump &d)
{
  d.write ("  %s %s:%d;\n",
	   m_type->get_debug_string (),
	   m_name->c_str (),
	   m_width);
}

/* Implementation of recording::memento::make_debug_string for
   results of new_bitfield.  */

recording::string *
recording::bitfield::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "%s:%d",
			      m_name->c_str (), m_width);
}

/* Implementation of recording::memento::write_reproducer for bitfields.  */

void
recording::bitfield::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "bitfield");
  r.write ("  gcc_jit_field *%s =\n"
	   "    gcc_jit_context_new_bitfield (%s,\n"
	   "                               %s, /* gcc_jit_location *loc */\n"
	   "                               %s, /* gcc_jit_type *type, */\n"
	   "                               %d, /* int width, */\n"
	   "                               %s); /* const char *name */\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   r.get_identifier_as_type (m_type),
	   m_width,
	   m_name->get_debug_string ());
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
  gcc_assert (m_fields == NULL);

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

const char *
recording::struct_::access_as_type (reproducer &r)
{
  return r.xstrdup_printf ("gcc_jit_struct_as_type (%s)",
			   r.get_identifier (this));
}

/* Implementation of recording::memento::make_debug_string for
   structs.  */

recording::string *
recording::struct_::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "struct %s", get_name ()->c_str ());
}

void
recording::struct_::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "struct");
  r.write ("  gcc_jit_struct *%s =\n"
	   "    gcc_jit_context_new_opaque_struct (%s,\n"
	   "                                       %s, /* gcc_jit_location *loc */\n"
	   "                                       %s); /* const char *name */\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (get_loc ()),
	   get_name ()->get_debug_string ());
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

/* Implementation of recording::memento::write_reproducer for unions.  */

void
recording::union_::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "union");

  const char *fields_id = r.make_tmp_identifier ("fields_for", this);
  r.write ("  gcc_jit_field *%s[%i] = {\n",
	   fields_id,
	   get_fields ()->length ());
  for (int i = 0; i < get_fields ()->length (); i++)
    r.write ("    %s,\n", r.get_identifier (get_fields ()->get_field (i)));
  r.write ("  };\n");

  r.write ("  gcc_jit_type *%s =\n"
	   "    gcc_jit_context_new_union_type (%s,\n"
	   "                                    %s, /* gcc_jit_location *loc */\n"
	   "                                    %s, /* const char *name */\n"
	   "                                    %i, /* int num_fields */\n"
	   "                                    %s); /* gcc_jit_field **fields */\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (get_loc ()),
	   get_name ()->get_debug_string (),
	   get_fields ()->length (),
	   fields_id);
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
  auto_vec<playback::field *> playback_fields;
  playback_fields.create (m_fields.length ());
  for (unsigned i = 0; i < m_fields.length (); i++)
    playback_fields.safe_push (m_fields[i]->playback_field ());
  m_struct_or_union->playback_compound_type ()->set_fields (&playback_fields);
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

/* Implementation of recording::memento::write_reproducer for the fields
   subclass.  */

void
recording::fields::write_reproducer (reproducer &r)
{
  if (m_struct_or_union)
    if (m_struct_or_union->dyn_cast_struct () == NULL)
      /* We have a union; the fields have already been written by
	 union::write_reproducer.  */
      return;

  const char *fields_id = r.make_identifier (this, "fields");
  r.write ("  gcc_jit_field *%s[%i] = {\n",
	   fields_id,
	   m_fields.length ());
  int i;
  field *field;
  FOR_EACH_VEC_ELT (m_fields, i, field)
    r.write ("    %s,\n", r.get_identifier (field));
  r.write ("  };\n");

  r.write ("  gcc_jit_struct_set_fields (%s, /* gcc_jit_struct *struct_type */\n"
	   "                             %s, /* gcc_jit_location *loc */\n"
	   "                             %i, /* int num_fields */\n"
	   "                             %s); /* gcc_jit_field **fields */\n",
	   r.get_identifier (m_struct_or_union),
	   r.get_identifier ((memento *)NULL),
	   m_fields.length (),
	   fields_id);
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

/* An rvalue visitor, for validating that every rvalue within an expression
   trees within "STMT" has the correct scope (e.g. no access to locals
   of a different function).  */

class rvalue_usage_validator : public recording::rvalue_visitor
{
 public:
  rvalue_usage_validator (const char *api_funcname,
			  recording::context *ctxt,
			  recording::statement *stmt);

  void
  visit (recording::rvalue *rvalue) final override;

 private:
  const char *m_api_funcname;
  recording::context *m_ctxt;
  recording::statement *m_stmt;
};

/* The trivial constructor for rvalue_usage_validator.  */

rvalue_usage_validator::rvalue_usage_validator (const char *api_funcname,
						recording::context *ctxt,
						recording::statement *stmt)
  : m_api_funcname (api_funcname),
    m_ctxt (ctxt),
    m_stmt (stmt)
{
}

/* Verify that the given rvalue is in the correct scope.  */

void
rvalue_usage_validator::visit (recording::rvalue *rvalue)
{
  gcc_assert (m_stmt->get_block ());
  recording::function *stmt_scope = m_stmt->get_block ()->get_function ();

  /* Most rvalues don't have a scope (only locals and params).  */
  if (rvalue->get_scope ())
    {
      if (rvalue->get_scope () != stmt_scope)
	m_ctxt->add_error
	  (rvalue->get_loc (),
	   "%s:"
	   " rvalue %s (type: %s)"
	   " has scope limited to function %s"
	   " but was used within function %s"
	   " (in statement: %s)",
	   m_api_funcname,
	   rvalue->get_debug_string (),
	   rvalue->get_type ()->get_debug_string (),
	   rvalue->get_scope ()->get_debug_string (),
	   stmt_scope->get_debug_string (),
	   m_stmt->get_debug_string ());
    }
  else
    {
      if (rvalue->dyn_cast_param ())
	m_ctxt->add_error
	  (rvalue->get_loc (),
	   "%s:"
	   " param %s (type: %s)"
	   " was used within function %s"
	   " (in statement: %s)"
	   " but is not associated with any function",
	   m_api_funcname,
	   rvalue->get_debug_string (),
	   rvalue->get_type ()->get_debug_string (),
	   stmt_scope->get_debug_string (),
	   m_stmt->get_debug_string ());
    }
}

/* Verify that it's valid to use this rvalue (and all expressions
   in the tree below it) within the given statement.

   For example, we must reject attempts to use a local from one
   function within a different function here, or we'll get
   an ICE deep inside toplev::main.  */

void
recording::rvalue::verify_valid_within_stmt (const char *api_funcname, statement *s)
{
  rvalue_usage_validator v (api_funcname,
			    s->get_context (),
			    s);

  /* Verify that it's OK to use this rvalue within s.  */
  v.visit (this);

  /* Traverse the expression tree below "this", verifying all rvalues
     within it.  */
  visit_children (&v);
}

/* Set the scope of this rvalue to be the given function.  This can only
   be done once on a given rvalue.  */

void
recording::rvalue::set_scope (function *scope)
{
  gcc_assert (scope);
  gcc_assert (m_scope == NULL);
  m_scope = scope;
}


/* Implementation of recording::rvalue::access_as_rvalue for rvalues
   themselves.
   Instances of rvalue don't need an upcast call.  */

const char *
recording::rvalue::access_as_rvalue (reproducer &r)
{
  return r.get_identifier (this);
}

/* Return a debug string for the given rvalue, wrapping it in parentheses
   if needed to mimic C's precedence rules, i.e. if OUTER_PREC is of
   stronger precedence that this rvalue's precedence.

   For example, given:

           MULT
          /    \
       PLUS     MINUS
      /    \   /     \
     A      B C       D

   we want to emit:

     (A + B) * (C - D)

   since MULT has strong precedence than PLUS and MINUS, whereas for:

           PLUS
          /    \
       MULT     DIVIDE
      /    \   /      \
     A      B C        D

   we can simply emit:

     A * B + C / D

   since PLUS has weaker precedence than MULT and DIVIDE.  */

const char *
recording::rvalue::get_debug_string_parens (enum precedence outer_prec)
{
  enum precedence this_prec = get_precedence ();

  /* If this_prec has stronger precedence than outer_prec, we don't
     need to wrap this in parens within the outer debug string.
     Stronger precedences occur earlier than weaker within the enum,
     so this is a less than test.  Equal precedences don't need
     parentheses.  */
  if (this_prec <= outer_prec)
    return get_debug_string();

  /* Otherwise, we need parentheses.  */

  /* Lazily-build and cache m_parenthesized_string.  */
  if (!m_parenthesized_string)
    {
      const char *debug_string = get_debug_string ();
      m_parenthesized_string = string::from_printf (get_context (),
						    "(%s)",
						    debug_string);
    }
  gcc_assert (m_parenthesized_string);
  return m_parenthesized_string->c_str ();
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

/* Implementation of recording::rvalue::access_as_rvalue for lvalues.
   Instances of lvalue need to be wrapped in a gcc_jit_lvalue_as_rvalue
   upcast call.  */

const char *
recording::lvalue::access_as_rvalue (reproducer &r)
{
  return r.xstrdup_printf ("gcc_jit_lvalue_as_rvalue (%s)",
			   r.get_identifier (this));
}

/* Implementation of recording::lvalue::access_as_lvalue for lvalues.
   Instances of lvalue don't need to be upcast.  */

const char *
recording::lvalue::access_as_lvalue (reproducer &r)
{
  return r.get_identifier (this);
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

void
recording::lvalue::set_tls_model (enum gcc_jit_tls_model model)
{
    m_tls_model = model;
}

void recording::lvalue::set_link_section (const char *name)
{
  m_link_section = new_string (name);
}

void recording::lvalue::set_register_name (const char *reg_name)
{
  m_reg_name = new_string (reg_name);
}

void recording::lvalue::set_alignment (unsigned bytes)
{
  m_alignment = bytes;
}

void recording::lvalue::add_string_attribute (
	gcc_jit_variable_attribute attribute,
	const char* value)
{
  m_string_attributes.push_back (std::make_pair (attribute, std::string (value)));
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

/* Implementation of recording::rvalue::access_as_rvalue for params.
   Instances of param need to be wrapped in a gcc_jit_param_as_rvalue
   upcast call.  */

const char *
recording::param::access_as_rvalue (reproducer &r)
{
  return r.xstrdup_printf ("gcc_jit_param_as_rvalue (%s)",
			   r.get_identifier (this));
}

/* Implementation of recording::lvalue::access_as_lvalue for params.
   Instances of param need to be wrapped in a gcc_jit_param_as_lvalue
   upcast call.  */

const char *
recording::param::access_as_lvalue (reproducer &r)
{
  return r.xstrdup_printf ("gcc_jit_param_as_lvalue (%s)",
			   r.get_identifier (this));
}

/* Implementation of recording::memento::write_reproducer for params. */

void
recording::param::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "param");
  r.write ("  gcc_jit_param *%s =\n"
	   "    gcc_jit_context_new_param (%s,\n"
	   "                               %s, /* gcc_jit_location *loc */\n"
	   "                               %s, /*gcc_jit_type *type */\n"
	   "                               %s); /* const char *name */\n",
	   id,
    r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   r.get_identifier_as_type (m_type),
	   m_name->get_debug_string ());
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
  m_blocks (),
  m_fn_ptr_type (NULL),
  m_attributes (),
  m_string_attributes (),
  m_int_array_attributes ()
{
  for (int i = 0; i< num_params; i++)
    {
      param *param = params[i];
      gcc_assert (param);

      /* Associate each param with this function.

	 Verify that the param doesn't already have a function.  */
      if (param->get_scope ())
	{
	  /* We've already rejected attempts to reuse a param between
	     different functions (within gcc_jit_context_new_function), so
	     if the param *does* already have a function, it must be being
	     reused within the params array for this function.  We must
	     produce an error for this reuse (blocking the compile), since
	     otherwise we'd have an ICE later on.  */
	  gcc_assert (this == param->get_scope ());
	  ctxt->add_error
	    (loc,
	     "gcc_jit_context_new_function:"
	     " parameter %s (type: %s)"
	     " is used more than once when creating function %s",
	     param->get_debug_string (),
	     param->get_type ()->get_debug_string (),
	     name->c_str ());
	}
      else
	{
	  /* The normal, non-error case: associate this function with the
	     param.  */
	  param->set_scope (this);
	}

      m_params.safe_push (param);
    }
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::function.  */

void
recording::function::replay_into (replayer *r)
{
  /* Convert m_params to a vec of playback param.  */
  auto_vec <playback::param *> params;
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
				     m_builtin_id,
				     m_attributes,
				     m_string_attributes,
				     m_int_array_attributes));
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
  for (auto attr: m_attributes)
  {
    const char* attribute = fn_attribute_to_string (attr);
    if (attribute)
      d.write ("__attribute(%s)__\n", attribute);
  }
  for (auto attr: m_string_attributes)
  {
    gcc_jit_fn_attribute& name = std::get<0>(attr);
    std::string& value = std::get<1>(attr);
    const char* attribute = fn_attribute_to_string (name);

    if (attribute)
      d.write ("__attribute(%s(\"%s\"))__\n", attribute, value.c_str());
  }
  for (auto attr: m_int_array_attributes)
  {
    gcc_jit_fn_attribute& name = std::get<0>(attr);
    std::vector<int>& values = std::get<1>(attr);
    const char* attribute = fn_attribute_to_string (name);
    if (attribute)
    {
      d.write ("__attribute(%s(", attribute);
      for (size_t i = 0; i < values.size(); ++i)
      {
	if (i > 0)
	  d.write (", %d", values[i]);
	else
	  d.write ("%d", values[i]);
      }
      d.write ("))__\n");
    }
  }

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
    if (m_blocks.length () == 0)
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
  if (!m_ctxt->get_inner_bool_option
        (INNER_BOOL_OPTION_ALLOW_UNREACHABLE_BLOCKS)
      && m_blocks.length () > 0 && num_invalid_blocks == 0)
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
	  vec <block *> successors = b->get_successor_blocks ();
	  int i;
	  block *succ;
	  FOR_EACH_VEC_ELT (successors, i, succ)
	    if (!succ->m_is_reachable)
	      worklist.safe_push (succ);
	  successors.release ();
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
  the_pp.set_output_stream (fp);

  pretty_printer *pp = &the_pp;

  pp_printf (pp, "digraph %s", get_debug_string ());
  pp_string (pp, " {\n");

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

  pp_string (pp, "}\n");
  pp_flush (pp);
  fclose (fp);
}

/* Implements the post-error-checking part of
   gcc_jit_function_get_address.  */

recording::rvalue *
recording::function::get_address (recording::location *loc)
{
  /* Lazily create and cache the function pointer type.  */
  if (!m_fn_ptr_type)
    {
      /* Make a recording::function_type for this function.  */
      auto_vec <recording::type *> param_types (m_params.length ());
      unsigned i;
      recording::param *param;
      FOR_EACH_VEC_ELT (m_params, i, param)
	param_types.safe_push (param->get_type ());
      recording::function_type *fn_type
	= m_ctxt->new_function_type (m_return_type,
				     m_params.length (),
				     param_types.address (),
				     m_is_variadic);
      m_fn_ptr_type = fn_type->get_pointer ();
    }
  gcc_assert (m_fn_ptr_type);

  rvalue *result = new function_pointer (get_context (), loc, this, m_fn_ptr_type);
  m_ctxt->record (result);
  return result;
}

void
recording::function::add_attribute (gcc_jit_fn_attribute attribute)
{
  m_attributes.push_back (attribute);
}

void
recording::function::add_string_attribute (gcc_jit_fn_attribute attribute,
					   const char* value)
{
  m_string_attributes.push_back (
	std::make_pair (attribute, std::string (value)));
}

void
recording::function::add_integer_array_attribute (
	gcc_jit_fn_attribute attribute,
	const int* value,
	size_t length)
{
  m_int_array_attributes.push_back (std::make_pair (
    attribute,
    std::vector<int> (value, value + length)));
}

/* Implementation of recording::memento::make_debug_string for
   functions.  */

recording::string *
recording::function::make_debug_string ()
{
  return m_name;
}

/* A table of enum gcc_jit_function_kind values expressed in string
   form.  */

static const char * const names_of_function_kinds[] = {
  "GCC_JIT_FUNCTION_EXPORTED",
  "GCC_JIT_FUNCTION_INTERNAL",
  "GCC_JIT_FUNCTION_IMPORTED",
  "GCC_JIT_FUNCTION_ALWAYS_INLINE"
};

/* Implementation of recording::memento::write_reproducer for functions. */

static const char * const fn_attribute_reproducer_strings[] =
{
  "GCC_JIT_FN_ATTRIBUTE_ALIAS",
  "GCC_JIT_FN_ATTRIBUTE_ALWAYS_INLINE",
  "GCC_JIT_FN_ATTRIBUTE_INLINE",
  "GCC_JIT_FN_ATTRIBUTE_NOINLINE",
  "GCC_JIT_FN_ATTRIBUTE_TARGET",
  "GCC_JIT_FN_ATTRIBUTE_USED",
  "GCC_JIT_FN_ATTRIBUTE_VISIBILITY",
  "GCC_JIT_FN_ATTRIBUTE_COLD",
  "GCC_JIT_FN_ATTRIBUTE_RETURNS_TWICE",
  "GCC_JIT_FN_ATTRIBUTE_PURE",
  "GCC_JIT_FN_ATTRIBUTE_CONST",
  "GCC_JIT_FN_ATTRIBUTE_WEAK",
  "GCC_JIT_FN_ATTRIBUTE_NONNULL",
};

std::string
get_vector_int_debug (std::vector<int> &values)
{
  std::stringstream s;

  s << "{";
  for(auto it = values.begin(); it != values.end(); ++it)
    {
      if (it != values.begin() )
	 s << ", ";
      s << *it;
    }
  s << "}";
  return s.str();
}

void
recording::function::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "func");

  if (m_builtin_id)
    {
      r.write ("  gcc_jit_function *%s =\n"
	       "    gcc_jit_context_get_builtin_function (%s,\n"
	       "                                          %s);\n",
	       id,
	       r.get_identifier (get_context ()),
	       m_name->get_debug_string ());
      return;
    }
  const char *params_id = r.make_tmp_identifier ("params_for", this);
  r.write ("  gcc_jit_param *%s[%i] = {\n",
	   params_id,
	   m_params.length ());
  int i;
  param *param;
  FOR_EACH_VEC_ELT (m_params, i, param)
    r.write ("    %s,\n", r.get_identifier (param));
  r.write ("  };\n");
  r.write ("  gcc_jit_function *%s =\n"
	   "    gcc_jit_context_new_function (%s, /* gcc_jit_context *ctxt */\n"
	   "                                  %s, /* gcc_jit_location *loc */\n"
	   "                                  %s, /* enum gcc_jit_function_kind kind */\n"
	   "                                  %s, /* gcc_jit_type *return_type */\n"
	   "                                  %s, /* const char *name */\n"
	   "                                  %i, /* int num_params */\n"
	   "                                  %s, /* gcc_jit_param **params */\n"
	   "                                  %i); /* int is_variadic */\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   names_of_function_kinds[m_kind],
	   r.get_identifier_as_type (m_return_type),
	   m_name->get_debug_string (),
	   m_params.length (),
	   params_id,
	   m_is_variadic);
  for (auto attribute : m_attributes)
    r.write("  gcc_jit_function_add_attribute (%s, %s);\n",
	    id,
	    fn_attribute_reproducer_strings[attribute]);
  for (auto attribute : m_string_attributes)
    r.write("  gcc_jit_function_add_string_attribute (%s, %s, \"%s\");\n",
	    id,
	    fn_attribute_reproducer_strings[std::get<0>(attribute)],
	    std::get<1>(attribute).c_str());
  for (auto attribute : m_int_array_attributes) {
    r.write("  gcc_jit_function_add_integer_array_attribute (%s,\n"
	    "                                                %s,\n"
	    "                                                (int[])%s,\n"
	    "                                                %lu);\n",
	    id,
	    fn_attribute_reproducer_strings[std::get<0>(attribute)],
	    get_vector_int_debug (std::get<1>(attribute)).c_str(),
	    std::get<1>(attribute).size ());
  }
}


/* The implementation of class gcc::jit::recording::block.  */

/* Create a recording::eval instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the heart of gcc_jit_block_add_eval.  */

recording::statement *
recording::block::add_eval (recording::location *loc,
			    recording::rvalue *rvalue)
{
  statement *result = new eval (this, loc, rvalue);
  m_ctxt->record (result);
  m_statements.safe_push (result);
  return result;
}

/* Create a recording::assignment instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the heart of gcc_jit_block_add_assignment.  */

recording::statement *
recording::block::add_assignment (recording::location *loc,
				  recording::lvalue *lvalue,
				  recording::rvalue *rvalue)
{
  statement *result = new assignment (this, loc, lvalue, rvalue);
  m_ctxt->record (result);
  m_statements.safe_push (result);
  return result;
}

/* Create a recording::assignment_op instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the heart of gcc_jit_block_add_assignment_op.  */

recording::statement *
recording::block::add_assignment_op (recording::location *loc,
				     recording::lvalue *lvalue,
				     enum gcc_jit_binary_op op,
				     recording::rvalue *rvalue)
{
  statement *result = new assignment_op (this, loc, lvalue, op, rvalue);
  m_ctxt->record (result);
  m_statements.safe_push (result);
  return result;
}

/* Create a recording::comment instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the heart of gcc_jit_block_add_comment.  */

recording::statement *
recording::block::add_comment (recording::location *loc,
			       const char *text)
{
  statement *result = new comment (this, loc, new_string (text));
  m_ctxt->record (result);
  m_statements.safe_push (result);
  return result;
}

/* Create a recording::extended_asm_simple instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the heart of gcc_jit_block_add_extended_asm.  */

recording::extended_asm *
recording::block::add_extended_asm (location *loc,
				    const char *asm_template)
{
  extended_asm *result
    = new extended_asm_simple (this, loc, new_string (asm_template));
  m_ctxt->record (result);
  m_statements.safe_push (result);
  return result;
}

/* Create a recording::end_with_conditional instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the heart of gcc_jit_block_end_with_conditional.  */

recording::statement *
recording::block::end_with_conditional (recording::location *loc,
					recording::rvalue *boolval,
					recording::block *on_true,
					recording::block *on_false)
{
  statement *result = new conditional (this, loc, boolval, on_true, on_false);
  m_ctxt->record (result);
  m_statements.safe_push (result);
  m_has_been_terminated = true;
  return result;
}

/* Create a recording::end_with_jump instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the heart of gcc_jit_block_end_with_jump.  */

recording::statement *
recording::block::end_with_jump (recording::location *loc,
				 recording::block *target)
{
  statement *result = new jump (this, loc, target);
  m_ctxt->record (result);
  m_statements.safe_push (result);
  m_has_been_terminated = true;
  return result;
}

/* Create a recording::end_with_return instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the post-error-checking parts of
   gcc_jit_block_end_with_return and
   gcc_jit_block_end_with_void_return.  */

recording::statement *
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
  return result;
}

/* Create a recording::switch_ instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the heart of gcc_jit_block_end_with_switch.  */

recording::statement *
recording::block::end_with_switch (recording::location *loc,
				   recording::rvalue *expr,
				   recording::block *default_block,
				   int num_cases,
				   recording::case_ **cases)
{
  statement *result = new switch_ (this, loc,
				   expr,
				   default_block,
				   num_cases,
				   cases);
  m_ctxt->record (result);
  m_statements.safe_push (result);
  m_has_been_terminated = true;
  return result;
}

/* Create a recording::extended_asm_goto instance and add it to
   the block's context's list of mementos, and to the block's
   list of statements.

   Implements the heart of gcc_jit_block_end_with_extended_asm_goto.  */


recording::extended_asm *
recording::block::end_with_extended_asm_goto (location *loc,
					      const char *asm_template,
					      int num_goto_blocks,
					      block **goto_blocks,
					      block *fallthrough_block)
{
  extended_asm *result
    = new extended_asm_goto (this, loc, new_string (asm_template),
			     num_goto_blocks, goto_blocks,
			     fallthrough_block);
  m_ctxt->record (result);
  m_statements.safe_push (result);
  m_has_been_terminated = true;
  return result;
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
  /* Check for termination.  */
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

/* Assuming that this block has been terminated, get the successor blocks
   as a vector.  Ownership of the vector transfers to the caller, which
   must call its release () method.

   Used when validating functions, and when dumping dot representations
   of them.  */

vec <recording::block *>
recording::block::get_successor_blocks () const
{
  gcc_assert (m_has_been_terminated);
  statement *last_statement = get_last_statement ();
  gcc_assert (last_statement);
  return last_statement->get_successor_blocks ();
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

/* Implementation of recording::memento::write_reproducer for blocks. */

void
recording::block::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "block");
  r.write ("  gcc_jit_block *%s =\n"
	   "    gcc_jit_function_new_block (%s, %s);\n",
	   id,
	   r.get_identifier (m_func),
	   m_name ? m_name->get_debug_string () : "NULL");
}

/* Disable warnings about missing quoting in GCC diagnostics for
   the pp_printf calls.  Their format strings deliberately don't
   follow GCC diagnostic conventions.  */
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
#endif

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

  pp_string (pp,
	     "}\"];\n\n");
  pp_flush (pp);
}

/* Dump the out-edges of the block in graphviz form into PP.  */

void
recording::block::dump_edges_to_dot (pretty_printer *pp)
{
  vec <block *> successors = get_successor_blocks ();
  int i;
  block *succ;
  FOR_EACH_VEC_ELT (successors, i, succ)
    pp_printf (pp,
	       "\tblock_%d:s -> block_%d:n;\n",
	       m_index, succ->m_index);
  successors.release ();
}

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif

namespace recording {
static const enum tls_model tls_models[] = {
  TLS_MODEL_NONE, /* GCC_JIT_TLS_MODEL_NONE */
  TLS_MODEL_GLOBAL_DYNAMIC, /* GCC_JIT_TLS_MODEL_GLOBAL_DYNAMIC */
  TLS_MODEL_LOCAL_DYNAMIC, /* GCC_JIT_TLS_MODEL_LOCAL_DYNAMIC */
  TLS_MODEL_INITIAL_EXEC, /* GCC_JIT_TLS_MODEL_INITIAL_EXEC */
  TLS_MODEL_LOCAL_EXEC, /* GCC_JIT_TLS_MODEL_LOCAL_EXEC */
};
} /* namespace recording */

/* The implementation of class gcc::jit::recording::global.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::global.  */

void
recording::global::replay_into (replayer *r)
{
  playback::lvalue *global = m_initializer
  ? r->new_global_initialized (playback_location (r, m_loc),
				 m_kind,
				 m_type->playback_type (),
				 m_type->dereference ()->get_size (),
				 m_initializer_num_bytes
				 / m_type->dereference ()->get_size (),
				 m_initializer,
				 playback_string (m_name),
				 m_flags,
				 m_string_attributes)
    : r->new_global (playback_location (r, m_loc),
		     m_kind,
		     m_type->playback_type (),
		     playback_string (m_name),
		     m_flags,
		     m_string_attributes);

  if (m_tls_model != GCC_JIT_TLS_MODEL_NONE)
    global->set_tls_model (recording::tls_models[m_tls_model]);

  if (m_link_section != NULL)
    global->set_link_section (m_link_section->c_str ());

  if (m_reg_name != NULL)
    global->set_register_name (m_reg_name->c_str ());

  if (m_alignment != 0)
    global->set_alignment (m_alignment);

  set_playback_obj (global);
}

/* Override the default implementation of
   recording::memento::write_to_dump for globals.
   This will be of the form:

   GCC_JIT_GLOBAL_EXPORTED:
      "TYPE NAME;"
      e.g. "int foo;"

   GCC_JIT_GLOBAL_INTERNAL:
      "static TYPE NAME;"
      e.g. "static int foo;"

   GCC_JIT_GLOBAL_IMPORTED:
      "extern TYPE NAME;"
      e.g. "extern int foo;"

   These are written to the top of the dump by
   recording::context::dump_to_file.  */

void
recording::global::write_to_dump (dump &d)
{
  if (d.update_locations ())
    m_loc = d.make_location ();

  switch (m_kind)
    {
    default:
      gcc_unreachable ();

    case GCC_JIT_GLOBAL_EXPORTED:
      break;

    case GCC_JIT_GLOBAL_INTERNAL:
      d.write ("static ");
      break;

    case GCC_JIT_GLOBAL_IMPORTED:
      d.write ("extern ");
      break;
    }

  for (auto attr: m_string_attributes)
  {
    gcc_jit_variable_attribute& name = std::get<0>(attr);
    std::string& value = std::get<1>(attr);
    const char* attribute = variable_attribute_to_string (name);

    if (attribute)
      d.write ("__attribute(%s(\"%s\"))__\n", attribute, value.c_str());
  }
  d.write ("%s %s",
	   m_type->get_debug_string (),
	   get_debug_string ());

  if (!m_initializer && !m_rvalue_init)
    {
      d.write (";\n");
    }
  else if (m_initializer)
    {
      d.write ("=\n  { ");
      const unsigned char *p = (const unsigned char *)m_initializer;
      for (size_t i = 0; i < m_initializer_num_bytes; i++)
	{
	  d.write ("0x%x, ", p[i]);
	  if (i && !(i % 64))
	    d.write ("\n    ");
	}
      d.write ("};\n");
    }
  else if (m_rvalue_init)
    {
      d.write (" = ");
      d.write ("%s", m_rvalue_init->get_debug_string ());
      d.write (";\n");
    }

  return;
}

/* A table of enum gcc_jit_global_kind values expressed in string
   form.  */

static const char * const global_kind_reproducer_strings[] = {
  "GCC_JIT_GLOBAL_EXPORTED",
  "GCC_JIT_GLOBAL_INTERNAL",
  "GCC_JIT_GLOBAL_IMPORTED"
};

template <typename T>
void
recording::global::write_initializer_reproducer (const char *id, reproducer &r)
{
  const char *init_id = r.make_tmp_identifier ("init_for", this);
  r.write ("  %s %s[] =\n    {",
	   m_type->dereference ()->get_debug_string (),
	   init_id);

  const T *p = (const T *)m_initializer;
  for (size_t i = 0; i < m_initializer_num_bytes / sizeof (T); i++)
    {
      r.write ("%" PRIu64 ", ", (uint64_t)p[i]);
      if (i && !(i % 64))
	r.write ("\n    ");
    }
  r.write ("};\n");
  r.write ("  gcc_jit_global_set_initializer (%s, %s, sizeof (%s));\n",
	   id, init_id, init_id);
}

/* Implementation of recording::memento::write_reproducer for globals. */

static const char * const tls_model_enum_strings[] = {
  "GCC_JIT_TLS_MODEL_NONE",
  "GCC_JIT_TLS_MODEL_GLOBAL_DYNAMIC",
  "GCC_JIT_TLS_MODEL_LOCAL_DYNAMIC",
  "GCC_JIT_TLS_MODEL_INITIAL_EXEC",
  "GCC_JIT_TLS_MODEL_LOCAL_EXEC",
};

static const char * const gcc_jit_variable_attribute_enum_strings[] = {
  "GCC_JIT_VARIABLE_ATTRIBUTE_VISIBILITY",
};

void
recording::global::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "block");
  r.write ("  gcc_jit_lvalue *%s =\n"
    "    gcc_jit_context_new_global (%s, /* gcc_jit_context *ctxt */\n"
    "                                %s, /* gcc_jit_location *loc */\n"
    "                                %s, /* enum gcc_jit_global_kind kind */\n"
    "                                %s, /* gcc_jit_type *type */\n"
    "                                %s); /* const char *name */\n",
    id,
    r.get_identifier (get_context ()),
    r.get_identifier (m_loc),
    global_kind_reproducer_strings[m_kind],
    r.get_identifier_as_type (get_type ()),
    m_name->get_debug_string ());

  if (m_tls_model != GCC_JIT_TLS_MODEL_NONE)
    r.write ("  gcc_jit_lvalue_set_tls_model (%s, /* gcc_jit_lvalue *lvalue */\n"
	     "                                %s); /* enum gcc_jit_tls_model model */\n",
	     id,
	     tls_model_enum_strings[m_tls_model]);

  if (m_link_section != NULL)
    r.write ("  gcc_jit_lvalue_set_link_section (%s, /* gcc_jit_lvalue *lvalue */\n"
	"                                  \"%s\"); /* */\n",
     id,
     m_link_section->c_str ());

  for (auto attribute : m_string_attributes)
    r.write("  gcc_jit_lvalue_add_string_attribute (%s, %s, \"%s\");\n",
	    id,
	    gcc_jit_variable_attribute_enum_strings[std::get<0>(attribute)],
	    std::get<1>(attribute).c_str());


  if (m_initializer)
    switch (m_type->dereference ()->get_size ())
      {
      case 1:
	write_initializer_reproducer<uint8_t> (id, r);
	break;
      case 2:
	write_initializer_reproducer<uint16_t> (id, r);
	break;
      case 4:
	write_initializer_reproducer<uint32_t> (id, r);
	break;
      case 8:
	write_initializer_reproducer<uint64_t> (id, r);
	break;
      default:
	/* This function is serving on sizes returned by 'get_size',
	   these are all covered by the previous cases.  */
	gcc_unreachable ();
      }
}

/* The implementation of the various const-handling classes:
   gcc::jit::recording::memento_of_new_rvalue_from_const <HOST_TYPE>.  */

/* Explicit specialization of the various mementos we're interested in.  */
template class recording::memento_of_new_rvalue_from_const <int>;
template class recording::memento_of_new_rvalue_from_const <long>;
template class recording::memento_of_new_rvalue_from_const <double>;
template class recording::memento_of_new_rvalue_from_const <void *>;

/* Implementation of the pure virtual hook recording::memento::replay_into
   for recording::memento_of_new_rvalue_from_const <HOST_TYPE>.  */

template <typename HOST_TYPE>
void
recording::
memento_of_new_rvalue_from_const <HOST_TYPE>::replay_into (replayer *r)
{
    set_playback_obj
      (r->new_rvalue_from_const <HOST_TYPE> (m_type->playback_type (),
					     m_value));
}

/* The make_debug_string and write_reproducer methods vary between the
   various
     memento_of_new_rvalue_from_const <HOST_TYPE>
   classes, so we explicitly write specializations of them.

   I (dmalcolm) find the code to be clearer if the "recording" vs "playback"
   namespaces are written out explicitly, which is why most of this file
   doesn't abbreviate things by entering the "recording" namespace.

   However, these specializations are required to be in the same namespace
   as the template, hence we now have to enter the gcc::jit::recording
   namespace.  */

namespace recording
{

/* The make_debug_string specialization for <int>, which renders it as
     (TARGET_TYPE)LITERAL
   e.g.
     "(int)42".  */

template <>
string *
memento_of_new_rvalue_from_const <int>::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "(%s)%i",
			      m_type->get_debug_string (),
			      m_value);
}

/* The get_wide_int specialization for <int>.  */

template <>
bool
memento_of_new_rvalue_from_const <int>::get_wide_int (wide_int *out) const
{
  *out = wi::shwi (m_value, sizeof (m_value) * 8);
  return true;
}

/* The write_reproducer specialization for <int>.  */

template <>
void
memento_of_new_rvalue_from_const <int>::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "rvalue");
  r.write ("  gcc_jit_rvalue *%s =\n"
    "    gcc_jit_context_new_rvalue_from_int (%s, /* gcc_jit_context *ctxt */\n"
    "                                         %s, /* gcc_jit_type *numeric_type */\n"
    "                                         %i); /* int value */\n",
    id,
    r.get_identifier (get_context ()),
    r.get_identifier_as_type (m_type),
    m_value);
}

/* The make_debug_string specialization for <long>, rendering it as
     (TARGET_TYPE)LITERAL
   e.g.
     "(long)42".  */

template <>
string *
memento_of_new_rvalue_from_const <long>::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "(%s)%li",
			      m_type->get_debug_string (),
			      m_value);
}

/* The get_wide_int specialization for <long>.  */

template <>
bool
memento_of_new_rvalue_from_const <long>::get_wide_int (wide_int *out) const
{
  *out = wi::shwi (m_value, sizeof (m_value) * 8);
  return true;
}

/* The write_reproducer specialization for <long>.  */

template <>
void
recording::memento_of_new_rvalue_from_const <long>::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "rvalue");

  /* We have to special-case LONG_MIN, since e.g.
       -9223372036854775808L
     is parsed as
       -(9223372036854775808L)
     and hence we'd get:
	error: integer constant is so large that it is unsigned [-Werror]
	Workaround this by writing (LONG_MIN + 1) - 1.  */
  if (m_value == LONG_MIN)
    {
      r.write ("  gcc_jit_rvalue *%s =\n"
	       "    gcc_jit_context_new_rvalue_from_long (%s, /* gcc_jit_context *ctxt */\n"
	       "                                          %s, /* gcc_jit_type *numeric_type */\n"
	       "                                          %ldL - 1); /* long value */\n",
	       id,
	       r.get_identifier (get_context ()),
	       r.get_identifier_as_type (m_type),
	       m_value + 1);
      return;
    }

  r.write ("  gcc_jit_rvalue *%s =\n"
	   "    gcc_jit_context_new_rvalue_from_long (%s, /* gcc_jit_context *ctxt */\n"
	   "                                          %s, /* gcc_jit_type *numeric_type */\n"
	   "                                          %ldL); /* long value */\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier_as_type (m_type),
	   m_value);
	   }

/* The make_debug_string specialization for <double>, rendering it as
     (TARGET_TYPE)LITERAL
   e.g.
     "(float)42.0".  */

template <>
string *
memento_of_new_rvalue_from_const <double>::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "(%s)%f",
			      m_type->get_debug_string (),
			      m_value);
}

/* The get_wide_int specialization for <double>.  */

template <>
bool
memento_of_new_rvalue_from_const <double>::get_wide_int (wide_int *) const
{
  return false;
}

/* The write_reproducer specialization for <double>.  */

template <>
void
recording::memento_of_new_rvalue_from_const <double>::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "rvalue");
  r.write ("  gcc_jit_rvalue *%s =\n"
    "    gcc_jit_context_new_rvalue_from_double (%s, /* gcc_jit_context *ctxt */\n"
    "                                            %s, /* gcc_jit_type *numeric_type */\n"
    "                                            %f); /* double value */\n",
    id,
    r.get_identifier (get_context ()),
    r.get_identifier_as_type (m_type),
    m_value);
}

/* The make_debug_string specialization for <void *>, rendering it as
     (TARGET_TYPE)HEX
   e.g.
     "(int *)0xdeadbeef"

   Zero is rendered as NULL e.g.
     "(int *)NULL".  */

template <>
string *
memento_of_new_rvalue_from_const <void *>::make_debug_string ()
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

/* The get_wide_int specialization for <void *>.  */

template <>
bool
memento_of_new_rvalue_from_const <void *>::get_wide_int (wide_int *) const
{
  return false;
}

/* Implementation of recording::memento::write_reproducer for <void *>
   values. */

template <>
void
memento_of_new_rvalue_from_const <void *>::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "rvalue");
  if (m_value)
    r.write ("  gcc_jit_rvalue *%s =\n"
	     "    gcc_jit_context_new_rvalue_from_ptr (%s, /* gcc_jit_context *ctxt */\n"
	     "                                         %s, /* gcc_jit_type *pointer_type */\n"
	     "                                         (void *)%p); /* void *value */\n",
	     id,
	     r.get_identifier (get_context ()),
	     r.get_identifier_as_type (m_type),
	     m_value);
  else
    r.write ("  gcc_jit_rvalue *%s =\n"
	     "    gcc_jit_context_null (%s, /* gcc_jit_context *ctxt */\n"
	     "                          %s); /* gcc_jit_type *pointer_type */\n",
	     id,
	     r.get_identifier (get_context ()),
	     r.get_identifier_as_type (m_type));
}

/* We're done specializing make_debug_string and write_reproducer, so we
   can exit the gcc::jit::recording namespace.  */

} // namespace recording

/* The implementation of class gcc::jit::recording::memento_of_typeinfo.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::memento_of_typeinfo.  */

void
recording::memento_of_typeinfo::replay_into (replayer *r)
{
  switch (m_info_type)
  {
  case TYPE_INFO_ALIGN_OF:
    set_playback_obj (r->new_alignof (m_type->playback_type ()));
    break;
  case TYPE_INFO_SIZE_OF:
    set_playback_obj (r->new_sizeof (m_type->playback_type ()));
    break;
  }
}

/* Implementation of recording::memento::make_debug_string for
   sizeof expressions.  */

recording::string *
recording::memento_of_typeinfo::make_debug_string ()
{
  const char* ident = "";
  switch (m_info_type)
  {
    case TYPE_INFO_ALIGN_OF:
      ident = "_Alignof";
      break;
    case TYPE_INFO_SIZE_OF:
      ident = "sizeof";
      break;
  }
  return string::from_printf (m_ctxt,
			      "%s (%s)",
			      ident,
			      m_type->get_debug_string ());
}

/* Implementation of recording::memento::write_reproducer for sizeof
   expressions.  */

void
recording::memento_of_typeinfo::write_reproducer (reproducer &r)
{
  const char* type = "";
  switch (m_info_type)
  {
    case TYPE_INFO_ALIGN_OF:
      type = "align";
      break;
    case TYPE_INFO_SIZE_OF:
      type = "size";
      break;
  }
  const char *id = r.make_identifier (this, "rvalue");
  r.write ("  gcc_jit_rvalue *%s =\n"
    "    gcc_jit_context_new_%sof (%s, /* gcc_jit_context *ctxt */\n"
    "                                (gcc_jit_type *) %s); /* gcc_jit_type *type */\n",
    id,
    type,
    r.get_identifier (get_context ()),
    r.get_identifier (m_type));
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
			      "%s",
			      m_value->get_debug_string ());
}

/* Implementation of recording::memento::write_reproducer for string literal
   values. */

void
recording::memento_of_new_string_literal::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "rvalue");
  r.write ("  gcc_jit_rvalue *%s =\n"
    "    gcc_jit_context_new_string_literal (%s, /* gcc_jit_context *ctxt */\n"
    "                                        %s); /* const char *value */\n",
    id,
    r.get_identifier (get_context ()),
    m_value->get_debug_string ());
}

/* The implementation of class
   gcc::jit::recording::memento_of_new_rvalue_from_vector.  */

/* The constructor for
   gcc::jit::recording::memento_of_new_rvalue_from_vector.  */

recording::memento_of_new_rvalue_from_vector::
memento_of_new_rvalue_from_vector (context *ctxt,
				   location *loc,
				   vector_type *type,
				   rvalue **elements)
: rvalue (ctxt, loc, type),
  m_vector_type (type),
  m_elements ()
{
  for (unsigned i = 0; i < type->get_num_units (); i++)
    m_elements.safe_push (elements[i]);
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::memento_of_new_rvalue_from_vector.  */

void
recording::memento_of_new_rvalue_from_vector::replay_into (replayer *r)
{
  auto_vec<playback::rvalue *> playback_elements;
  playback_elements.create (m_elements.length ());
  for (unsigned i = 0; i< m_elements.length (); i++)
    playback_elements.safe_push (m_elements[i]->playback_rvalue ());

  set_playback_obj (r->new_rvalue_from_vector (playback_location (r, m_loc),
					       m_type->playback_type (),
					       playback_elements));
}

/* Implementation of pure virtual hook recording::rvalue::visit_children
   for recording::memento_of_new_rvalue_from_vector.  */

void
recording::memento_of_new_rvalue_from_vector::visit_children (rvalue_visitor *v)
{
  for (unsigned i = 0; i< m_elements.length (); i++)
    v->visit (m_elements[i]);
}

/* Implementation of recording::memento::make_debug_string for
   vectors.  */

recording::string *
recording::memento_of_new_rvalue_from_vector::make_debug_string ()
{
  comma_separated_string elements (m_elements, get_precedence ());

  /* Now build a string.  */
  string *result = string::from_printf (m_ctxt,
					"{%s}",
					elements.as_char_ptr ());

 return result;

}

/* Implementation of recording::memento::write_reproducer for
   vectors.  */

void
recording::memento_of_new_rvalue_from_vector::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "vector");
  const char *elements_id = r.make_tmp_identifier ("elements_for_", this);
  r.write ("  gcc_jit_rvalue *%s[%i] = {\n",
	   elements_id,
	   m_elements.length ());
  for (unsigned i = 0; i< m_elements.length (); i++)
    r.write ("    %s,\n", r.get_identifier_as_rvalue (m_elements[i]));
  r.write ("  };\n");
  r.write ("  gcc_jit_rvalue *%s =\n"
	   "    gcc_jit_context_new_rvalue_from_vector (%s, /* gcc_jit_context *ctxt */\n"
	   "                                            %s, /* gcc_jit_location *loc */\n"
	   "                                            %s, /* gcc_jit_type *vec_type */\n"
	   "                                            %i, /* size_t num_elements  */ \n"
	   "                                            %s); /* gcc_jit_rvalue **elements*/\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   r.get_identifier (m_vector_type),
	   m_elements.length (),
	   elements_id);
}

void
recording::ctor::visit_children (rvalue_visitor *v)
{
  for (unsigned int i = 0; i < m_values.length (); i++)
    v->visit (m_values[i]);
}

recording::string *
recording::ctor::make_debug_string ()
{
  //Make a compound literal-ish
  pretty_printer pp;

  pp_string (&pp, "(");
  pp_string (&pp, m_type->get_debug_string ());
  pp_string (&pp, ") {");

  size_t field_n = m_fields.length ();
  size_t values_n = m_values.length ();

  if (!field_n && !values_n)
    ;
  else if (!field_n && values_n)
    {
      for (size_t i = 0; i < values_n; i++)
	{
	  if (m_values[i])
	    pp_string (&pp, m_values[i]->get_debug_string ());
	  else
	    pp_string (&pp, "0");
	  if (i + 1 != values_n)
	    pp_string (&pp, ", ");
	}
    }
  else if (field_n && values_n)
    {
      for (size_t i = 0; i < values_n; i++)
	{
	  pp_string (&pp, ".");
	  pp_string (&pp, m_fields[i]->get_debug_string ());
	  pp_string (&pp, "=");
	  if (m_values[i])
	    pp_string (&pp, m_values[i]->get_debug_string ());
	  else
	    pp_string (&pp, "0");
	  if (i + 1 != values_n)
	    pp_string (&pp, ", ");
	}
    }
  /* m_fields are never populated with m_values empty.  */

  pp_string (&pp, "}");

  return new_string (pp_formatted_text (&pp));
}

void
recording::ctor::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "rvalue");
  type *type = get_type ();

  r.write ("  gcc_jit_rvalue *%s;\n", id);
  r.write ("  {\n"); /* Open scope for locals.  */

  if (type->is_union ())
    {
      if (m_values.length () == 0)
	r.write ("    gcc_jit_rvalue *value = NULL;\n");
      else
	r.write ("    gcc_jit_rvalue *value = %s;\n",
		 r.get_identifier (m_values[0]));

      if (m_fields.length () == 0)
	r.write ("    gcc_jit_field *field = NULL;\n");
      else
	r.write ("    gcc_jit_field *field = %s;\n",
		 r.get_identifier (m_fields[0]));
    }
  else
    {
      /* Write the array of values.  */
      if (m_values.length () == 0)
	r.write ("    gcc_jit_rvalue **values = NULL;\n");
      else
	{
	  r.write ("    gcc_jit_rvalue *values[] = {\n");
	  for (size_t i = 0; i < m_values.length (); i++)
	    r.write ("        %s,\n", r.get_identifier (m_values[i]));
	  r.write ("      };\n");
	}
      /* Write the array of fields.  */
      if (m_fields.length () == 0)
	r.write ("    gcc_jit_field **fields = NULL;\n");
      else
	{
	  r.write ("    gcc_jit_field *fields[] = {\n");
	  for (size_t i = 0; i < m_fields.length (); i++)
	    r.write ("        %s,\n", r.get_identifier (m_fields[i]));
	  r.write ("      };\n");
	}
    }
  if (type->is_array ())
    r.write (
"    %s =\n"
"      gcc_jit_context_new_array_constructor (%s,\n"
"                                             %s, /* gcc_jit_location *loc */\n"
"                                             %s, /* gcc_jit_type *type */\n"
"                                             %i, /* int num_values */\n"
"                                             values);\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   r.get_identifier_as_type (get_type ()),
	   m_values.length ());
  else if (type->is_struct ())
    r.write (
"    %s =\n"
"      gcc_jit_context_new_struct_constructor (%s,\n"
"                                              %s, /* loc */\n"
"                                              %s, /* gcc_jit_type *type */\n"
"                                              %i, /* int num_values */\n"
"                                              fields,\n"
"                                              values);\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   r.get_identifier_as_type (get_type ()),
	   m_values.length ());
  else if (type->is_union ())
    r.write (
"    %s =\n"
"      gcc_jit_context_new_union_constructor (%s,\n"
"                                             %s, /* loc */\n"
"                                             %s, /* gcc_jit_type *type */\n"
"                                             field,\n"
"                                             value);\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   r.get_identifier_as_type (get_type ()));
  else
    gcc_unreachable ();

  r.write ("  }\n"); /* Close scope for locals.  */
}

void
recording::ctor::replay_into (replayer *r)
{
  auto_vec<playback::rvalue *> playback_values;
  auto_vec<playback::field *> playback_fields;

  int n = m_values.length ();

  type *type = get_type ();

  /* Handle arrays, and return.  */
  if (type->is_array ())
    {
      playback_values.reserve (n, false);

      for (int i = 0; i < n; i++)
	{
	  /* null m_values element indicates zero ctor.  */
	  playback_values.quick_push (m_values[i] ?
				      m_values[i]->playback_rvalue () :
				      NULL);
	}
      set_playback_obj (r->new_ctor (playback_location (r, m_loc),
				     get_type ()->playback_type (),
				     NULL,
				     &playback_values));
      return;
    }
  /* ... else handle unions and structs.  */

  playback_values.reserve (n, false);
  playback_fields.reserve (n, false);

  for (int i = 0; i < n; i++)
    {
      /* null m_values element indicates zero ctor.  */
      playback_values.quick_push (m_values[i] ?
				    m_values[i]->playback_rvalue () :
				    NULL);
      playback_fields.quick_push (m_fields[i]->playback_field ());
    }

  set_playback_obj (r->new_ctor (playback_location (r, m_loc),
				 get_type ()->playback_type (),
				 &playback_fields,
				 &playback_values));
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

/* Implementation of pure virtual hook recording::rvalue::visit_children
   for recording::unary_op.  */
void
recording::unary_op::visit_children (rvalue_visitor *v)
{
  v->visit (m_a);
}

/* Implementation of recording::memento::make_debug_string for
   unary ops.  */

static const char * const unary_op_strings[] = {
  "-", /* GCC_JIT_UNARY_OP_MINUS */
  "~", /* GCC_JIT_UNARY_OP_BITWISE_NEGATE */
  "!", /* GCC_JIT_UNARY_OP_LOGICAL_NEGATE */
  "abs ", /* GCC_JIT_UNARY_OP_ABS */
};

recording::string *
recording::unary_op::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "%s(%s)",
			      unary_op_strings[m_op],
			      m_a->get_debug_string ());
}

const char * const unary_op_reproducer_strings[] = {
  "GCC_JIT_UNARY_OP_MINUS",
  "GCC_JIT_UNARY_OP_BITWISE_NEGATE",
  "GCC_JIT_UNARY_OP_LOGICAL_NEGATE",
  "GCC_JIT_UNARY_OP_ABS"
};

/* Implementation of recording::memento::write_reproducer for unary ops.  */

void
recording::unary_op::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "rvalue");
  r.write ("  gcc_jit_rvalue *%s =\n"
	   "    gcc_jit_context_new_unary_op (%s,\n"
	   "                                  %s, /* gcc_jit_location *loc */\n"
	   "                                  %s, /* enum gcc_jit_unary_op op */\n"
	   "                                  %s, /* gcc_jit_type *result_type */\n"
	   "                                  %s); /* gcc_jit_rvalue *a */\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   unary_op_reproducer_strings[m_op],
	   r.get_identifier_as_type (get_type ()),
	   r.get_identifier_as_rvalue (m_a));
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

/* Implementation of pure virtual hook recording::rvalue::visit_children
   for recording::binary_op.  */
void
recording::binary_op::visit_children (rvalue_visitor *v)
{
  v->visit (m_a);
  v->visit (m_b);
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
  enum precedence prec = get_precedence ();
  return string::from_printf (m_ctxt,
			      "%s %s %s",
			      m_a->get_debug_string_parens (prec),
			      binary_op_strings[m_op],
			      m_b->get_debug_string_parens (prec));
}

const char * const binary_op_reproducer_strings[] = {
  "GCC_JIT_BINARY_OP_PLUS",
  "GCC_JIT_BINARY_OP_MINUS",
  "GCC_JIT_BINARY_OP_MULT",
  "GCC_JIT_BINARY_OP_DIVIDE",
  "GCC_JIT_BINARY_OP_MODULO",
  "GCC_JIT_BINARY_OP_BITWISE_AND",
  "GCC_JIT_BINARY_OP_BITWISE_XOR",
  "GCC_JIT_BINARY_OP_BITWISE_OR",
  "GCC_JIT_BINARY_OP_LOGICAL_AND",
  "GCC_JIT_BINARY_OP_LOGICAL_OR",
  "GCC_JIT_BINARY_OP_LSHIFT",
  "GCC_JIT_BINARY_OP_RSHIFT"
};

/* Implementation of recording::memento::write_reproducer for binary ops.  */

void
recording::binary_op::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "rvalue");
  r.write ("  gcc_jit_rvalue *%s =\n"
	   "    gcc_jit_context_new_binary_op (%s,\n"
	   "                                   %s, /* gcc_jit_location *loc */\n"
	   "                                   %s, /* enum gcc_jit_binary_op op */\n"
	   "                                   %s, /* gcc_jit_type *result_type */\n"
	   "                                   %s, /* gcc_jit_rvalue *a */\n"
	   "                                   %s); /* gcc_jit_rvalue *b */\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   binary_op_reproducer_strings[m_op],
	   r.get_identifier_as_type (get_type ()),
	   r.get_identifier_as_rvalue (m_a),
	   r.get_identifier_as_rvalue (m_b));
}

namespace recording {
static const enum precedence binary_op_precedence[] = {
  PRECEDENCE_ADDITIVE, /* GCC_JIT_BINARY_OP_PLUS */
  PRECEDENCE_ADDITIVE, /* GCC_JIT_BINARY_OP_MINUS */

  PRECEDENCE_MULTIPLICATIVE, /* GCC_JIT_BINARY_OP_MULT */
  PRECEDENCE_MULTIPLICATIVE, /* GCC_JIT_BINARY_OP_DIVIDE */
  PRECEDENCE_MULTIPLICATIVE, /* GCC_JIT_BINARY_OP_MODULO */

  PRECEDENCE_BITWISE_AND, /* GCC_JIT_BINARY_OP_BITWISE_AND */
  PRECEDENCE_BITWISE_XOR, /* GCC_JIT_BINARY_OP_BITWISE_XOR */
  PRECEDENCE_BITWISE_IOR, /* GCC_JIT_BINARY_OP_BITWISE_OR */
  PRECEDENCE_LOGICAL_AND, /* GCC_JIT_BINARY_OP_LOGICAL_AND */
  PRECEDENCE_LOGICAL_OR, /* GCC_JIT_BINARY_OP_LOGICAL_OR */
  PRECEDENCE_SHIFT, /* GCC_JIT_BINARY_OP_LSHIFT */
  PRECEDENCE_SHIFT, /* GCC_JIT_BINARY_OP_RSHIFT */
};
} /* namespace recording */

enum recording::precedence
recording::binary_op::get_precedence () const
{
  return binary_op_precedence[m_op];
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
  enum precedence prec = get_precedence ();
  return string::from_printf (m_ctxt,
			      "%s %s %s",
			      m_a->get_debug_string_parens (prec),
			      comparison_strings[m_op],
			      m_b->get_debug_string_parens (prec));
}

/* A table of enum gcc_jit_comparison values expressed in string
   form.  */

static const char * const comparison_reproducer_strings[] =
{
  "GCC_JIT_COMPARISON_EQ",
  "GCC_JIT_COMPARISON_NE",
  "GCC_JIT_COMPARISON_LT",
  "GCC_JIT_COMPARISON_LE",
  "GCC_JIT_COMPARISON_GT",
  "GCC_JIT_COMPARISON_GE"
};

/* Implementation of recording::memento::write_reproducer for comparisons.  */

void
recording::comparison::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "rvalue");
  r.write ("  gcc_jit_rvalue *%s =\n"
	   "    gcc_jit_context_new_comparison (%s,\n"
	   "                                    %s, /* gcc_jit_location *loc */\n"
	   "                                    %s, /* enum gcc_jit_comparison op */\n"
	   "                                    %s, /* gcc_jit_rvalue *a */\n"
	   "                                    %s); /* gcc_jit_rvalue *b */\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   comparison_reproducer_strings[m_op],
	   r.get_identifier_as_rvalue (m_a),
	   r.get_identifier_as_rvalue (m_b));
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::comparison.  */

void
recording::comparison::replay_into (replayer *r)
{
  set_playback_obj (r->new_comparison (playback_location (r, m_loc),
				       m_op,
				       m_a->playback_rvalue (),
				       m_b->playback_rvalue (),
				       m_type->playback_type ()));
}

/* Implementation of pure virtual hook recording::rvalue::visit_children
   for recording::comparison.  */

void
recording::comparison::visit_children (rvalue_visitor *v)
{
  v->visit (m_a);
  v->visit (m_b);
}

namespace recording {
static const enum precedence comparison_precedence[] =
{
  PRECEDENCE_EQUALITY, /* GCC_JIT_COMPARISON_EQ */
  PRECEDENCE_EQUALITY, /* GCC_JIT_COMPARISON_NE */

  PRECEDENCE_RELATIONAL,  /* GCC_JIT_COMPARISON_LT */
  PRECEDENCE_RELATIONAL, /* GCC_JIT_COMPARISON_LE */
  PRECEDENCE_RELATIONAL,  /* GCC_JIT_COMPARISON_GT */
  PRECEDENCE_RELATIONAL, /* GCC_JIT_COMPARISON_GE */
};
} /* namespace recording */

enum recording::precedence
recording::comparison::get_precedence () const
{
  return comparison_precedence[m_op];
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

/* Implementation of pure virtual hook recording::rvalue::visit_children
   for recording::cast.  */
void
recording::cast::visit_children (rvalue_visitor *v)
{
  v->visit (m_rvalue);
}

/* Implementation of recording::memento::make_debug_string for
   casts.  */

recording::string *
recording::cast::make_debug_string ()
{
  enum precedence prec = get_precedence ();
  return string::from_printf (m_ctxt,
			      "(%s)%s",
			      get_type ()->get_debug_string (),
			      m_rvalue->get_debug_string_parens (prec));
}

/* Implementation of recording::memento::write_reproducer for casts.  */

void
recording::cast::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "rvalue");
  r.write ("  gcc_jit_rvalue *%s =\n"
	   "    gcc_jit_context_new_cast (%s,\n"
	   "                              %s, /* gcc_jit_location *loc */\n"
	   "                              %s, /* gcc_jit_rvalue *rvalue */\n"
	   "                              %s); /* gcc_jit_type *type */\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   r.get_identifier_as_rvalue (m_rvalue),
	   r.get_identifier_as_type (get_type ()));
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::bitcast.  */

void
recording::bitcast::replay_into (replayer *r)
{
  set_playback_obj (r->new_bitcast (playback_location (r, m_loc),
				    m_rvalue->playback_rvalue (),
				    get_type ()->playback_type ()));
}

/* Implementation of pure virtual hook recording::rvalue::visit_children
   for recording::bitcast.  */
void
recording::bitcast::visit_children (rvalue_visitor *v)
{
  v->visit (m_rvalue);
}

/* Implementation of recording::memento::make_debug_string for
   casts.  */

recording::string *
recording::bitcast::make_debug_string ()
{
  enum precedence prec = get_precedence ();
  return string::from_printf (m_ctxt,
			      "bitcast(%s, %s)",
			      m_rvalue->get_debug_string_parens (prec),
			      get_type ()->get_debug_string ());
}

/* Implementation of recording::memento::write_reproducer for casts.  */

void
recording::bitcast::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "rvalue");
  r.write ("  gcc_jit_rvalue *%s =\n"
	   "    gcc_jit_context_new_bitcast (%s,\n"
	   "                                 %s, /* gcc_jit_location *loc */\n"
	   "                                 %s, /* gcc_jit_rvalue *rvalue */\n"
	   "                                 %s); /* gcc_jit_type *type */\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   r.get_identifier_as_rvalue (m_rvalue),
	   r.get_identifier_as_type (get_type ()));
}

/* The implementation of class gcc::jit::recording::base_call.  */

/* The constructor for gcc::jit::recording::base_call.  */

recording::base_call::base_call (context *ctxt,
				 location *loc,
				 type *type_,
				 int numargs,
				 rvalue **args)
: rvalue (ctxt, loc, type_),
  m_args (),
  m_require_tail_call (0)
{
  for (int i = 0; i< numargs; i++)
    m_args.safe_push (args[i]);
}

/* Subroutine for use by call and call_though_ptr's write_reproducer
   methods.  */

void
recording::base_call::write_reproducer_tail_call (reproducer &r,
						  const char *id)
{
  if (m_require_tail_call)
    {
      r.write ("  gcc_jit_rvalue_set_bool_require_tail_call (%s,  /* gcc_jit_rvalue *call*/\n"
	       "                                             %i); /* int require_tail_call*/\n",
	       id,
	       1);
    }
}

/* The implementation of class gcc::jit::recording::call.  */

/* The constructor for gcc::jit::recording::call.  */

recording::call::call (recording::context *ctxt,
		       recording::location *loc,
		       recording::function *func,
		       int numargs,
		       rvalue **args)
: base_call (ctxt, loc, func->get_return_type (), numargs, args),
  m_func (func)
{
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::call.  */

void
recording::call::replay_into (replayer *r)
{
  auto_vec<playback::rvalue *> playback_args;
  playback_args.create (m_args.length ());
  for (unsigned i = 0; i< m_args.length (); i++)
    playback_args.safe_push (m_args[i]->playback_rvalue ());

  set_playback_obj (r->new_call (playback_location (r, m_loc),
				 m_func->playback_function (),
				 &playback_args,
				 m_require_tail_call));
}

/* Implementation of pure virtual hook recording::rvalue::visit_children
   for recording::call.  */

void
recording::call::visit_children (rvalue_visitor *v)
{
  for (unsigned i = 0; i< m_args.length (); i++)
    v->visit (m_args[i]);
}

/* Implementation of recording::memento::make_debug_string for
   function calls.  */

recording::string *
recording::call::make_debug_string ()
{
  /* First, build a buffer for the arguments.  */
  comma_separated_string args (m_args, get_precedence ());

  /* ...and use it to get the string for the call as a whole.  */
  string *result = string::from_printf (m_ctxt,
					"%s (%s)",
					m_func->get_debug_string (),
					args.as_char_ptr ());

  return result;
}

void
recording::call::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "call");
  const char *args_id = r.make_tmp_identifier ("args_for_", this);
  r.write ("  gcc_jit_rvalue *%s[%i] = {\n",
	   args_id,
	   m_args.length ());
  for (unsigned i = 0; i< m_args.length (); i++)
    r.write ("    %s,\n", r.get_identifier_as_rvalue (m_args[i]));
  r.write ("  };\n");
  r.write ("  gcc_jit_rvalue *%s =\n"
	   "    gcc_jit_context_new_call (%s, /* gcc_jit_context *ctxt */\n"
	   "                              %s, /* gcc_jit_location *loc */\n"
	   "                              %s, /* gcc_jit_function *func */\n"
	   "                              %i, /* int numargs  */ \n"
	   "                              %s); /* gcc_jit_rvalue **args*/\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   r.get_identifier (m_func),
	   m_args.length (),
	   args_id);
  write_reproducer_tail_call (r, id);
}

/* The implementation of class gcc::jit::recording::call_through_ptr.  */

/* The constructor for recording::call_through_ptr. */

recording::call_through_ptr::call_through_ptr (recording::context *ctxt,
					       recording::location *loc,
					       recording::rvalue *fn_ptr,
					       int numargs,
					       rvalue **args)
: base_call (ctxt, loc,
	     fn_ptr->get_type ()->dereference ()
	       ->as_a_function_type ()->get_return_type (),
	     numargs, args),
  m_fn_ptr (fn_ptr)
{
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::call_through_ptr.  */

void
recording::call_through_ptr::replay_into (replayer *r)
{
  auto_vec<playback::rvalue *> playback_args;
  playback_args.create (m_args.length ());
  for (unsigned i = 0; i< m_args.length (); i++)
    playback_args.safe_push (m_args[i]->playback_rvalue ());

  set_playback_obj (r->new_call_through_ptr (playback_location (r, m_loc),
					     m_fn_ptr->playback_rvalue (),
					     &playback_args,
					     m_require_tail_call));
}

/* Implementation of pure virtual hook recording::rvalue::visit_children
   for recording::call_through_ptr.  */

void
recording::call_through_ptr::visit_children (rvalue_visitor *v)
{
  v->visit (m_fn_ptr);
  for (unsigned i = 0; i< m_args.length (); i++)
    v->visit (m_args[i]);
}

/* Implementation of recording::memento::make_debug_string for
   calls through function ptrs.  */

recording::string *
recording::call_through_ptr::make_debug_string ()
{
  enum precedence prec = get_precedence ();
  /* First, build a buffer for the arguments.  */
  /* Calculate length of said buffer.  */
  size_t sz = 1; /* nil terminator */
  for (unsigned i = 0; i< m_args.length (); i++)
    {
      sz += strlen (m_args[i]->get_debug_string_parens (prec));
      sz += 2; /* ", " separator */
    }

  /* Now allocate and populate the buffer.  */
  char *argbuf = new char[sz];
  size_t len = 0;

  for (unsigned i = 0; i< m_args.length (); i++)
    {
      strcpy (argbuf + len, m_args[i]->get_debug_string_parens (prec));
      len += strlen (m_args[i]->get_debug_string_parens (prec));
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
					m_fn_ptr->get_debug_string_parens (prec),
					argbuf);

  delete[] argbuf;

  return result;
}

/* Implementation of recording::memento::write_reproducer for
   call_through_ptr.  */

void
recording::call_through_ptr::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "call");
  const char *args_id = r.make_tmp_identifier ("args_for_", this);
  r.write ("  gcc_jit_rvalue *%s[%i] = {\n",
	     args_id,
	     m_args.length ());
  for (unsigned i = 0; i< m_args.length (); i++)
    r.write ("    %s,\n", r.get_identifier_as_rvalue (m_args[i]));
  r.write ("  };\n");
  r.write ("  gcc_jit_rvalue *%s =\n"
	   "    gcc_jit_context_new_call_through_ptr (%s, /* gcc_jit_context *ctxt */\n"
	   "                              %s, /* gcc_jit_location *loc */\n"
	   "                              %s, /* gcc_jit_rvalue *fn_ptr */\n"
	   "                              %i, /* int numargs  */ \n"
	   "                              %s); /* gcc_jit_rvalue **args*/\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   r.get_identifier_as_rvalue (m_fn_ptr),
	   m_args.length (),
	   args_id);
  write_reproducer_tail_call (r, id);
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

/* Implementation of pure virtual hook recording::rvalue::visit_children
   for recording::array_access.  */

void
recording::array_access::visit_children (rvalue_visitor *v)
{
  v->visit (m_ptr);
  v->visit (m_index);
}

/* Implementation of recording::memento::make_debug_string for
   array accesses.  */

recording::string *
recording::array_access::make_debug_string ()
{
  enum precedence prec = get_precedence ();
  return string::from_printf (m_ctxt,
			      "%s[%s]",
			      m_ptr->get_debug_string_parens (prec),
			      m_index->get_debug_string_parens (prec));
}

/* Implementation of recording::memento::write_reproducer for
   array_access.  */

void
recording::array_access::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "lvalue");
  r.write ("  gcc_jit_lvalue *%s = \n"
	   "    gcc_jit_context_new_array_access (%s, /* gcc_jit_context *ctxt */\n"
	   "                                      %s, /*gcc_jit_location *loc */\n"
	   "                                      %s, /* gcc_jit_rvalue *ptr */\n"
	   "                                      %s); /* gcc_jit_rvalue *index */\n",
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   r.get_identifier_as_rvalue (m_ptr),
	   r.get_identifier_as_rvalue (m_index));
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

/* Implementation of pure virtual hook recording::rvalue::visit_children
   for recording::access_field_of_lvalue.  */

void
recording::access_field_of_lvalue::visit_children (rvalue_visitor *v)
{
  v->visit (m_lvalue);
}

/* Implementation of recording::memento::make_debug_string for
   accessing a field of an lvalue.  */

recording::string *
recording::access_field_of_lvalue::make_debug_string ()
{
  enum precedence prec = get_precedence ();
  return string::from_printf (m_ctxt,
			      "%s.%s",
			      m_lvalue->get_debug_string_parens (prec),
			      m_field->get_debug_string ());
}

/* Implementation of recording::memento::write_reproducer for
   access_field_of_lvalue.  */

void
recording::access_field_of_lvalue::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "lvalue");
  r.write ("  gcc_jit_lvalue *%s = \n"
	   "    gcc_jit_lvalue_access_field (%s, /*gcc_jit_lvalue *struct_or_union */\n"
	   "                                 %s, /*gcc_jit_location *loc */\n"
	   "                                 %s);\n",
	   id,
	   r.get_identifier_as_lvalue (m_lvalue),
	   r.get_identifier (m_loc),
	   r.get_identifier (m_field));
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

/* Implementation of pure virtual hook recording::rvalue::visit_children
   for recording::access_field_rvalue.  */

void
recording::access_field_rvalue::visit_children (rvalue_visitor *v)
{
  v->visit (m_rvalue);
}

/* Implementation of recording::memento::make_debug_string for
   accessing a field of an rvalue.  */

recording::string *
recording::access_field_rvalue::make_debug_string ()
{
  enum precedence prec = get_precedence ();
  return string::from_printf (m_ctxt,
			      "%s.%s",
			      m_rvalue->get_debug_string_parens (prec),
			      m_field->get_debug_string ());
}

/* Implementation of recording::memento::write_reproducer for
   access_field_rvalue.  */

void
recording::access_field_rvalue::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "rvalue");
  r.write ("  gcc_jit_rvalue *%s = \n"
	   "    gcc_jit_rvalue_access_field (%s, /*gcc_jit_rvalue *struct_or_union */\n"
	   "                                 %s, /*gcc_jit_location *loc */\n"
	   "                                 %s);\n",
	   id,
	   r.get_identifier_as_rvalue (m_rvalue),
	   r.get_identifier (m_loc),
	   r.get_identifier (m_field));
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

/* Implementation of pure virtual hook recording::rvalue::visit_children
   for recording::dereference_field_rvalue.  */

void
recording::dereference_field_rvalue::visit_children (rvalue_visitor *v)
{
  v->visit (m_rvalue);
}

/* Implementation of recording::memento::make_debug_string for
   dereferencing a field of an rvalue.  */

recording::string *
recording::dereference_field_rvalue::make_debug_string ()
{
  enum precedence prec = get_precedence ();
  return string::from_printf (m_ctxt,
			      "%s->%s",
			      m_rvalue->get_debug_string_parens (prec),
			      m_field->get_debug_string ());
}

/* Implementation of recording::memento::write_reproducer for
   dereference_field_rvalue.  */

void
recording::dereference_field_rvalue::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "lvalue");
  r.write ("  gcc_jit_lvalue *%s=\n"
	   "    gcc_jit_rvalue_dereference_field (%s, /* gcc_jit_rvalue *ptr */\n"
	   "                                      %s, /* gcc_jit_location *loc */\n"
	   "                                      %s); /* gcc_jit_field *field */\n",
	   id,
	   r.get_identifier_as_rvalue (m_rvalue),
	   r.get_identifier (m_loc),
	   r.get_identifier (m_field));
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

/* Implementation of pure virtual hook recording::rvalue::visit_children
   for recording::dereference_rvalue.  */

void
recording::dereference_rvalue::visit_children (rvalue_visitor *v)
{
  v->visit (m_rvalue);
}

/* Implementation of recording::memento::make_debug_string for
   dereferencing an rvalue.  */

recording::string *
recording::dereference_rvalue::make_debug_string ()
{
  enum precedence prec = get_precedence ();
  return string::from_printf (m_ctxt,
			      "*%s",
			      m_rvalue->get_debug_string_parens (prec));
}

/* Implementation of recording::memento::write_reproducer for
   dereference_rvalue.  */

void
recording::dereference_rvalue::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "dereference");
  r.write ("  gcc_jit_lvalue *%s =\n"
	   "    gcc_jit_rvalue_dereference (%s, /* gcc_jit_rvalue *rvalue */\n"
	   "                                %s); /* gcc_jit_location *loc */\n",
	   id,
	   r.get_identifier_as_rvalue (m_rvalue),
	   r.get_identifier (m_loc));
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

/* Implementation of pure virtual hook recording::rvalue::visit_children
   for recording::get_address_of_lvalue.  */

void
recording::get_address_of_lvalue::visit_children (rvalue_visitor *v)
{
  v->visit (m_lvalue);
}

/* Implementation of recording::memento::make_debug_string for
   getting the address of an lvalue.  */

recording::string *
recording::get_address_of_lvalue::make_debug_string ()
{
  enum precedence prec = get_precedence ();
  return string::from_printf (m_ctxt,
			      "&%s",
			      m_lvalue->get_debug_string_parens (prec));
}

/* Implementation of recording::memento::write_reproducer for
   get_address_of_lvalue.  */

void
recording::get_address_of_lvalue::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "address_of");
  r.write ("  gcc_jit_rvalue *%s =\n"
	   "    gcc_jit_lvalue_get_address (%s, /* gcc_jit_lvalue *lvalue */\n"
	   "                                %s); /* gcc_jit_location *loc */\n",
	   id,
	   r.get_identifier_as_lvalue (m_lvalue),
	   r.get_identifier (m_loc));
}

/* The implementation of class gcc::jit::recording::function_pointer.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::function_pointer.  */

void
recording::function_pointer::replay_into (replayer *r)
{
  set_playback_obj (
    m_fn->playback_function ()->
      get_address (playback_location (r, m_loc)));
}

void
recording::function_pointer::visit_children (rvalue_visitor *)
{
  /* Empty.  */
}

/* Implementation of recording::memento::make_debug_string for
   getting the address of an lvalue.  */

recording::string *
recording::function_pointer::make_debug_string ()
{
  return string::from_printf (m_ctxt,
			      "%s",
			      m_fn->get_debug_string ());
}

/* Implementation of recording::memento::write_reproducer for
   function_pointer.  */

void
recording::function_pointer::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "address_of");
  r.write ("  gcc_jit_rvalue *%s =\n"
	   "    gcc_jit_function_get_address (%s, /* gcc_jit_function *fn */\n"
	   "                                  %s); /* gcc_jit_location *loc */\n",
	   id,
	   r.get_identifier (m_fn),
	   r.get_identifier (m_loc));
}

/* The implementation of class gcc::jit::recording::local.  */

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::local.  */

void
recording::local::replay_into (replayer *r)
{
  playback::lvalue *obj = m_func->playback_function ()
      ->new_local (playback_location (r, m_loc),
		   m_type->playback_type (),
		   playback_string (m_name),
		   m_string_attributes);

  if (m_reg_name != NULL)
    obj->set_register_name (m_reg_name->c_str ());

  if (m_alignment != 0)
    obj->set_alignment (m_alignment);

  set_playback_obj (obj);
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
  d.write ("  %s %s;\n",
	   m_type->get_debug_string (),
	   get_debug_string ());
}

void
recording::local::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "local");
  r.write ("  gcc_jit_lvalue *%s =\n"
	   "    gcc_jit_function_new_local (%s, /* gcc_jit_function *func */\n"
	   "                                %s, /* gcc_jit_location *loc */\n"
	   "                                %s, /* gcc_jit_type *type */\n"
	   "                                %s); /* const char *name */\n",
	   id,
	   r.get_identifier (m_func),
	   r.get_identifier (m_loc),
	   r.get_identifier_as_type (m_type),
	   m_name->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::statement.  */

/* We poison the default implementation of
   gcc::jit::recording::statement::get_successor_blocks
   since this vfunc must only ever be called on terminator
   statements.  */

vec <recording::block *>
recording::statement::get_successor_blocks () const
{
  /* The base class implementation is for non-terminating statements,
     and thus should never be called.  */
  gcc_unreachable ();
  vec <block *> result;
  result.create (0);
  return result;
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

/* Implementation of recording::memento::write_reproducer for
   eval statements.  */

void
recording::eval::write_reproducer (reproducer &r)
{
  r.write ("  gcc_jit_block_add_eval (%s, /*gcc_jit_block *block */\n"
	   "                          %s, /* gcc_jit_location *loc */\n"
	   "                          %s); /* gcc_jit_rvalue *rvalue */\n",
	   r.get_identifier (get_block ()),
	   r.get_identifier (get_loc ()),
	   r.get_identifier_as_rvalue (m_rvalue));
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

/* Implementation of recording::memento::write_reproducer for
   assignment statements.  */

void
recording::assignment::write_reproducer (reproducer &r)
{
  r.write ("  gcc_jit_block_add_assignment (%s, /*gcc_jit_block *block */\n"
	   "                                %s, /* gcc_jit_location *loc */\n"
	   "                                %s, /* gcc_jit_lvalue *lvalue */\n"
	   "                                %s); /* gcc_jit_rvalue *rvalue */\n",
	   r.get_identifier (get_block ()),
	   r.get_identifier (get_loc ()),
	   r.get_identifier_as_lvalue (m_lvalue),
	   r.get_identifier_as_rvalue (m_rvalue));
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

/* Implementation of recording::memento::write_reproducer for
   assignment_op statements.  */

void
recording::assignment_op::write_reproducer (reproducer &r)
{
  r.write ("  gcc_jit_block_add_assignment_op (%s, /*gcc_jit_block *block */\n"
	   "                                   %s, /* gcc_jit_location *loc */\n"
	   "                                   %s, /* gcc_jit_lvalue *lvalue */\n"
	   "                                   %s, /* enum gcc_jit_binary_op op */\n"
	   "                                   %s); /* gcc_jit_rvalue *rvalue */\n",
	   r.get_identifier (get_block ()),
	   r.get_identifier (get_loc ()),
	   r.get_identifier_as_lvalue (m_lvalue),
	   binary_op_reproducer_strings[m_op],
	   r.get_identifier_as_rvalue (m_rvalue));
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

/* Implementation of recording::memento::write_reproducer for
   comments.  */

void
recording::comment::write_reproducer (reproducer &r)
{
  r.write ("  gcc_jit_block_add_comment (%s, /*gcc_jit_block *block */\n"
	   "                             %s, /* gcc_jit_location *loc */\n"
	   "                             %s); /* const char *text */\n",
	   r.get_identifier (get_block ()),
	   r.get_identifier (get_loc ()),
	   m_text->get_debug_string ());
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

vec <recording::block *>
recording::conditional::get_successor_blocks () const
{
  vec <block *> result;
  result.create (2);
  result.quick_push (m_on_true);
  result.quick_push (m_on_false);
  return result;
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

/* Implementation of recording::memento::write_reproducer for
   conditional statements.  */

void
recording::conditional::write_reproducer (reproducer &r)
{
  r.write ("  gcc_jit_block_end_with_conditional (%s, /*gcc_jit_block *block */\n"
	   "                                      %s, /* gcc_jit_location *loc */\n"
	   "                                      %s, /* gcc_jit_rvalue *boolval */\n"
	   "                                      %s, /* gcc_jit_block *on_true */\n"
	   "                                      %s); /* gcc_jit_block *on_false */\n",
	   r.get_identifier (get_block ()),
	   r.get_identifier (get_loc ()),
	   r.get_identifier_as_rvalue (m_boolval),
	   r.get_identifier (m_on_true),
	   r.get_identifier (m_on_false));
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

vec <recording::block *>
recording::jump::get_successor_blocks () const
{
  vec <block *> result;
  result.create (1);
  result.quick_push (m_target);
  return result;
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

/* Implementation of recording::memento::write_reproducer for
   jump statements.  */

void
recording::jump::write_reproducer (reproducer &r)
{
  r.write ("  gcc_jit_block_end_with_jump (%s, /*gcc_jit_block *block */\n"
	   "                               %s, /* gcc_jit_location *loc */\n"
	   "                               %s); /* gcc_jit_block *target */\n",
	   r.get_identifier (get_block ()),
	   r.get_identifier (get_loc ()),
	   r.get_identifier (m_target));
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

vec <recording::block *>
recording::return_::get_successor_blocks () const
{
  vec <block *> result;
  result.create (0);
  return result;
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

/* Implementation of recording::memento::write_reproducer for
   return statements.  */

void
recording::return_::write_reproducer (reproducer &r)
{
  if (m_rvalue)
    r.write ("  gcc_jit_block_end_with_return (%s, /*gcc_jit_block *block */\n"
	     "                                 %s, /* gcc_jit_location *loc */\n"
	     "                                 %s); /* gcc_jit_rvalue *rvalue */\n",
	     r.get_identifier (get_block ()),
	     r.get_identifier (get_loc ()),
	     r.get_identifier_as_rvalue (m_rvalue));
  else
    r.write ("  gcc_jit_block_end_with_void_return (%s, /*gcc_jit_block *block */\n"
	     "                                      %s); /* gcc_jit_location *loc */\n",
	     r.get_identifier (get_block ()),
	     r.get_identifier (get_loc ()));
}

/* The implementation of class gcc::jit::recording::case_.  */

void
recording::case_::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "case");
  const char *fmt =
    "  gcc_jit_case *%s = \n"
    "    gcc_jit_context_new_case (%s, /*gcc_jit_context *ctxt */\n"
    "                              %s, /* gcc_jit_rvalue *min_value */\n"
    "                              %s, /* gcc_jit_rvalue *max_value */\n"
    "                              %s); /* gcc_jit_block *dest_block */\n";
  r.write (fmt,
	   id,
	   r.get_identifier (get_context ()),
	   r.get_identifier_as_rvalue (m_min_value),
	   r.get_identifier_as_rvalue (m_max_value),
	   r.get_identifier (m_dest_block));
}

recording::string *
recording::case_::make_debug_string ()
{
  return string::from_printf (get_context (),
			      "case %s ... %s: goto %s;",
			      m_min_value->get_debug_string (),
			      m_max_value->get_debug_string (),
			      m_dest_block->get_debug_string ());
}

/* The implementation of class gcc::jit::recording::switch_.  */

/* gcc::jit::recording::switch_'s constructor.  */

recording::switch_::switch_ (block *b,
			     location *loc,
			     rvalue *expr,
			     block *default_block,
			     int num_cases,
			     case_ **cases)
: statement (b, loc),
  m_expr (expr),
  m_default_block (default_block)
{
  m_cases.reserve_exact (num_cases);
  for (int i = 0; i< num_cases; i++)
    m_cases.quick_push (cases[i]);
}

/* Implementation of pure virtual hook recording::memento::replay_into
   for recording::switch_.  */

void
recording::switch_::replay_into (replayer *r)
{
  auto_vec <playback::case_> pcases;
  int i;
  recording::case_ *rcase;
  pcases.reserve_exact (m_cases.length ());
  FOR_EACH_VEC_ELT (m_cases, i, rcase)
    {
      playback::case_ pcase (rcase->get_min_value ()->playback_rvalue (),
			     rcase->get_max_value ()->playback_rvalue (),
			     rcase->get_dest_block ()->playback_block ());
      pcases.safe_push (pcase);
    }
  playback_block (get_block ())
    ->add_switch (playback_location (r),
		  m_expr->playback_rvalue (),
		  m_default_block->playback_block (),
		  &pcases);
}

/* Override the poisoned default implementation of
   gcc::jit::recording::statement::get_successor_blocks

   A switch statement has (NUM_CASES + 1) successor blocks.  */

vec <recording::block *>
recording::switch_::get_successor_blocks () const
{
  vec <block *> result;
  result.create (m_cases.length () + 1);
  result.quick_push (m_default_block);
  int i;
  case_ *c;
  FOR_EACH_VEC_ELT (m_cases, i, c)
    result.quick_push (c->get_dest_block ());
  return result;
}

/* Implementation of recording::memento::make_debug_string for
   a switch statement.  */

recording::string *
recording::switch_::make_debug_string ()
{
  auto_vec <char> cases_str;
  int i;
  case_ *c;
  FOR_EACH_VEC_ELT (m_cases, i, c)
    {
      size_t len = strlen (c->get_debug_string ());
      unsigned idx = cases_str.length ();
      cases_str.safe_grow (idx + 1 + len, true);
      cases_str[idx] = ' ';
      memcpy (&(cases_str[idx + 1]),
	      c->get_debug_string (),
	      len);
    }
  cases_str.safe_push ('\0');

  return string::from_printf (m_ctxt,
			      "switch (%s) {default: goto %s;%s}",
			      m_expr->get_debug_string (),
			      m_default_block->get_debug_string (),
			      &cases_str[0]);
}

/* Implementation of recording::memento::write_reproducer for
   switch statements.  */

void
recording::switch_::write_reproducer (reproducer &r)
{
  r.make_identifier (this, "switch");
  int i;
  case_ *c;
  const char *cases_id =
    r.make_tmp_identifier ("cases_for", this);
  r.write ("  gcc_jit_case *%s[%i] = {\n",
	   cases_id,
	   m_cases.length ());
  FOR_EACH_VEC_ELT (m_cases, i, c)
    r.write ("    %s,\n", r.get_identifier (c));
  r.write ("  };\n");
  const char *fmt =
    "  gcc_jit_block_end_with_switch (%s, /*gcc_jit_block *block */\n"
    "                                 %s, /* gcc_jit_location *loc */\n"
    "                                 %s, /* gcc_jit_rvalue *expr */\n"
    "                                 %s, /* gcc_jit_block *default_block */\n"
    "                                 %i, /* int num_cases */\n"
    "                                 %s); /* gcc_jit_case **cases */\n";
    r.write (fmt,
	     r.get_identifier (get_block ()),
	     r.get_identifier (get_loc ()),
	     r.get_identifier_as_rvalue (m_expr),
	     r.get_identifier (m_default_block),
	     m_cases.length (),
	     cases_id);
}

/* class asm_operand : public memento.  */

recording::asm_operand::asm_operand (extended_asm *ext_asm,
				     string *asm_symbolic_name,
				     string *constraint)
: memento (ext_asm->get_context ()),
  m_ext_asm (ext_asm),
  m_asm_symbolic_name (asm_symbolic_name),
  m_constraint (constraint)
{
}

void
recording::asm_operand::print (pretty_printer *pp) const
{
  if (m_asm_symbolic_name)
    {
      pp_character (pp, '[');
      pp_string (pp, m_asm_symbolic_name->c_str ());
      pp_character (pp, ']');
      pp_space (pp);
    }
  pp_string (pp, m_constraint->get_debug_string ());
  /* Subclass will add lvalue/rvalue.  */
}

recording::string *
recording::asm_operand::make_debug_string ()
{
  pretty_printer pp;
  print (&pp);
  return m_ctxt->new_string (pp_formatted_text (&pp), false);
}

/* class output_asm_operand : public asm_operand.  */

void
recording::output_asm_operand::write_reproducer (reproducer &r)
{
  const char *fmt =
    "  gcc_jit_extended_asm_add_output_operand (%s, /* gcc_jit_extended_asm *ext_asm */\n"
    "                                           %s, /* const char *asm_symbolic_name */\n"
    "                                           %s, /* const char *constraint */\n"
    "                                           %s); /* gcc_jit_lvalue *dest */\n";
  r.write (fmt,
	   r.get_identifier (m_ext_asm),
	   (m_asm_symbolic_name
	    ? m_asm_symbolic_name->get_debug_string () : "NULL"),
	   m_constraint->get_debug_string (),
	   r.get_identifier (m_dest));
}

void
recording::output_asm_operand::print (pretty_printer *pp) const
{
  asm_operand::print (pp);
  pp_string (pp, " (");
  pp_string (pp, m_dest->get_debug_string ());
  pp_string (pp, ")");
}

/* class input_asm_operand : public asm_operand.  */

void
recording::input_asm_operand::write_reproducer (reproducer &r)
{
  const char *fmt =
    "  gcc_jit_extended_asm_add_input_operand (%s, /* gcc_jit_extended_asm *ext_asm */\n"
    "                                          %s, /* const char *asm_symbolic_name */\n"
    "                                          %s, /* const char *constraint */\n"
    "                                          %s); /* gcc_jit_rvalue *src */\n";
  r.write (fmt,
	   r.get_identifier (m_ext_asm),
	   (m_asm_symbolic_name
	    ? m_asm_symbolic_name->get_debug_string () : "NULL"),
	   m_constraint->get_debug_string (),
	   r.get_identifier_as_rvalue (m_src));
}

void
recording::input_asm_operand::print (pretty_printer *pp) const
{
  asm_operand::print (pp);
  pp_string (pp, " (");
  pp_string (pp, m_src->get_debug_string ());
  pp_string (pp, ")");
}

/* The implementation of class gcc::jit::recording::extended_asm.  */

void
recording::extended_asm::add_output_operand (const char *asm_symbolic_name,
					     const char *constraint,
					     lvalue *dest)
{
  output_asm_operand *op
    = new output_asm_operand (this,
			      new_string (asm_symbolic_name),
			      new_string (constraint),
			      dest);
  m_ctxt->record (op);
  m_output_ops.safe_push (op);
}

void
recording::extended_asm::add_input_operand (const char *asm_symbolic_name,
					    const char *constraint,
					    rvalue *src)
{
  input_asm_operand *op
    = new input_asm_operand (this,
			     new_string (asm_symbolic_name),
			     new_string (constraint),
			     src);
  m_ctxt->record (op);
  m_input_ops.safe_push (op);
}

void
recording::extended_asm::add_clobber (const char *victim)
{
  m_clobbers.safe_push (new_string (victim));
}

/* Implementation of recording::memento::replay_into
   for recording::extended_asm.  */

void
recording::extended_asm::replay_into (replayer *r)
{
  auto_vec<playback::asm_operand> playback_output_ops;
  auto_vec<playback::asm_operand> playback_input_ops;
  auto_vec<const char *> playback_clobbers;
  auto_vec<playback::block *> playback_goto_blocks;

  /* Populate outputs.  */
  {
    output_asm_operand *rec_asm_op;
    unsigned i;
    FOR_EACH_VEC_ELT (m_output_ops, i, rec_asm_op)
      {
	playback::asm_operand playback_asm_op
	  (rec_asm_op->get_symbolic_name (),
	   rec_asm_op->get_constraint (),
	   rec_asm_op->get_lvalue ()->playback_lvalue ()->as_tree ());
	playback_output_ops.safe_push (playback_asm_op);
      }
  }

  /* Populate inputs.  */
  {
    input_asm_operand *rec_asm_op;
    unsigned i;
    FOR_EACH_VEC_ELT (m_input_ops, i, rec_asm_op)
      {
	playback::asm_operand playback_asm_op
	  (rec_asm_op->get_symbolic_name (),
	   rec_asm_op->get_constraint (),
	   rec_asm_op->get_rvalue ()->playback_rvalue ()->as_tree ());
	playback_input_ops.safe_push (playback_asm_op);
      }
  }

  /* Populate clobbers.  */
  {
    string *rec_clobber;
    unsigned i;
    FOR_EACH_VEC_ELT (m_clobbers, i, rec_clobber)
      playback_clobbers.safe_push (rec_clobber->c_str ());
  }

  /* Populate playback blocks if an "asm goto".  */
  maybe_populate_playback_blocks (&playback_goto_blocks);

  playback_block (get_block ())
    ->add_extended_asm (playback_location (r),
			m_asm_template->c_str (),
			m_is_volatile, m_is_inline,
			&playback_output_ops,
			&playback_input_ops,
			&playback_clobbers,
			&playback_goto_blocks);
}

/* Implementation of recording::memento::make_debug_string for
   an extended_asm "statement".  */

recording::string *
recording::extended_asm::make_debug_string ()
{
  pretty_printer pp;
  pp_string (&pp, "asm ");
  if (m_is_volatile)
    pp_string (&pp, "volatile ");
  if (m_is_inline)
    pp_string (&pp, "inline ");
  if (is_goto ())
    pp_string (&pp, "goto ");
  pp_character (&pp, '(');
  pp_string (&pp, m_asm_template->get_debug_string ());
  pp_string (&pp, " : ");
  unsigned i;
  {
    output_asm_operand *asm_op;
    FOR_EACH_VEC_ELT (m_output_ops, i, asm_op)
      {
	if (i > 0)
	  pp_string (&pp, ", ");
	asm_op->print (&pp);
      }
  }
  pp_string (&pp, " : ");
  {
    input_asm_operand *asm_op;
    FOR_EACH_VEC_ELT (m_input_ops, i, asm_op)
      {
	if (i > 0)
	  pp_string (&pp, ", ");
	asm_op->print (&pp);
      }
  }
  pp_string (&pp, " : ");
  string *rec_clobber;
  FOR_EACH_VEC_ELT (m_clobbers, i, rec_clobber)
      {
	if (i > 0)
	  pp_string (&pp, ", ");
	pp_string (&pp, rec_clobber->get_debug_string ());
      }
  maybe_print_gotos (&pp);
  pp_character (&pp, ')');
  return new_string (pp_formatted_text (&pp));
}

void
recording::extended_asm::write_flags (reproducer &r)
{
  if (m_is_volatile)
    r.write ("  gcc_jit_extended_asm_set_volatile_flag (%s, 1);\n",
	     r.get_identifier (this));
  if (m_is_inline)
    r.write ("  gcc_jit_extended_asm_set_inline_flag (%s, 1);\n",
	     r.get_identifier (this));
}

void
recording::extended_asm::write_clobbers (reproducer &r)
{
  string *clobber;
  unsigned i;
  FOR_EACH_VEC_ELT (m_clobbers, i, clobber)
    r.write ("  gcc_jit_extended_asm_add_clobber (%s, %s);\n",
	     r.get_identifier (this),
	     clobber->get_debug_string ());
}

/* Implementation of recording::memento::write_reproducer for
   extended_asm_simple.  */

void
recording::extended_asm_simple::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "extended_asm");
  r.write ("  gcc_jit_extended_asm *%s =\n"
	   "    gcc_jit_block_add_extended_asm (%s, /*gcc_jit_block *block */\n"
	   "                                    %s, /* gcc_jit_location *loc */\n"
	   "                                    %s); /* const char *asm_template */\n",
	   id,
	   r.get_identifier (get_block ()),
	   r.get_identifier (get_loc ()),
	   m_asm_template->get_debug_string ());
  write_flags (r);
  write_clobbers (r);
}

void
recording::extended_asm::
maybe_populate_playback_blocks (auto_vec <playback::block *> *)
{
  /* Do nothing; not an "asm goto".  */
}

/* The implementation of class gcc::jit::recording::extended_asm_goto.  */

/* recording::extended_asm_goto's ctor.  */

recording::extended_asm_goto::extended_asm_goto (block *b,
						 location *loc,
						 string *asm_template,
						 int num_goto_blocks,
						 block **goto_blocks,
						 block *fallthrough_block)
: extended_asm (b, loc, asm_template),
  m_goto_blocks (num_goto_blocks),
  m_fallthrough_block (fallthrough_block)
{
  for (int i = 0; i < num_goto_blocks; i++)
    m_goto_blocks.quick_push (goto_blocks[i]);
}

/* Implementation of recording::memento::replay_into
   for recording::extended_asm_goto.  */

void
recording::extended_asm_goto::replay_into (replayer *r)
{
  /* Chain up to base class impl.  */
  recording::extended_asm::replay_into (r);

  /* ...and potentially add a goto for the fallthrough.  */
  if (m_fallthrough_block)
    playback_block (get_block ())
      ->add_jump (playback_location (r),
		  m_fallthrough_block->playback_block ());
}

/* Implementation of recording::memento::write_reproducer for
   extended_asm_goto.  */

void
recording::extended_asm_goto::write_reproducer (reproducer &r)
{
  const char *id = r.make_identifier (this, "extended_asm");
  const char *blocks_id = r.make_tmp_identifier ("blocks_for", this);
  r.write ("  gcc_jit_block *%s[%i] = {\n",
	   blocks_id,
	   m_goto_blocks.length ());
  int i;
  block *b;
  FOR_EACH_VEC_ELT (m_goto_blocks, i, b)
    r.write ("    %s,\n", r.get_identifier (b));
  r.write ("  };\n");
  r.write ("  gcc_jit_extended_asm *%s =\n"
	   "    gcc_jit_block_end_with_extended_asm_goto (%s, /*gcc_jit_block *block */\n"
	   "                                              %s, /* gcc_jit_location *loc */\n"
	   "                                              %s, /* const char *asm_template */\n"
	   "                                              %i, /* int num_goto_blocks */\n"
	   "                                              %s, /* gcc_jit_block **goto_blocks */\n"
	   "                                              %s); /* gcc_jit_block *fallthrough_block */\n",
	   id,
	   r.get_identifier (get_block ()),
	   r.get_identifier (get_loc ()),
	   m_asm_template->get_debug_string (),
	   m_goto_blocks.length (),
	   blocks_id,
	   (m_fallthrough_block
	    ? r.get_identifier (m_fallthrough_block)
	    : "NULL"));
  write_flags (r);
  write_clobbers (r);
}

/* Override the poisoned default implementation of
   gcc::jit::recording::statement::get_successor_blocks

   An extended_asm_goto can jump to the m_goto_blocks, and to
   the (optional) m_fallthrough_block.  */

vec <recording::block *>
recording::extended_asm_goto::get_successor_blocks () const
{
  vec <block *> result;
  result.create (m_goto_blocks.length () + 1);
  if (m_fallthrough_block)
    result.quick_push (m_fallthrough_block);
  result.splice (m_goto_blocks);
  return result;
}

/* Vfunc for use by recording::extended_asm::make_debug_string.  */

void
recording::extended_asm_goto::maybe_print_gotos (pretty_printer *pp) const
{
  pp_string (pp, " : ");
  unsigned i;
  block *b;
  FOR_EACH_VEC_ELT (m_goto_blocks, i, b)
    {
      if (i > 0)
	pp_string (pp, ", ");
      pp_string (pp, b->get_debug_string ());
    }
  /* Non-C syntax here.  */
  if (m_fallthrough_block)
    pp_printf (pp, " [fallthrough: %s]",
	       m_fallthrough_block->get_debug_string ());
}

/* Vfunc for use by recording::extended_asm::replay_into.  */

void
recording::extended_asm_goto::
maybe_populate_playback_blocks (auto_vec <playback::block *> *out)
{
  unsigned i;
  block *b;
  FOR_EACH_VEC_ELT (m_goto_blocks, i, b)
    out->safe_push (b->playback_block ());
}

/* class top_level_asm : public memento.  */

recording::top_level_asm::top_level_asm (context *ctxt,
					 location *loc,
					 string *asm_stmts)
: memento (ctxt),
  m_loc (loc),
  m_asm_stmts (asm_stmts)
{
}

/* Implementation of recording::memento::replay_into for top-level asm.  */

void
recording::top_level_asm::replay_into (replayer *r)
{
  r->add_top_level_asm (m_asm_stmts->c_str ());
}

/* Implementation of recording::memento::make_debug_string for
   top-level asm.  */

recording::string *
recording::top_level_asm::make_debug_string ()
{
  return string::from_printf (m_ctxt, "asm (%s)",
			      m_asm_stmts->get_debug_string ());
}

/* Override the default implementation of
   recording::memento::write_to_dump.
   Don't indent the string.  */

void
recording::top_level_asm::write_to_dump (dump &d)
{
  d.write ("%s;\n", get_debug_string ());
}

/* Implementation of recording::memento::write_reproducer for top-level asm. */

void
recording::top_level_asm::write_reproducer (reproducer &r)
{
  r.write ("  gcc_jit_context_add_top_level_asm (%s, /* gcc_jit_context *ctxt */\n"
	   "                                     %s, /* gcc_jit_location *loc */\n"
	   "                                     %s); /* const char *asm_stmts */\n",
	   r.get_identifier (get_context ()),
	   r.get_identifier (m_loc),
	   m_asm_stmts->get_debug_string ());
}

void
recording::global_init_rvalue::replay_into (replayer *r)
{
  r->global_set_init_rvalue (m_variable->playback_lvalue (),
			     m_init->playback_rvalue ());
}

void
recording::global_init_rvalue::write_reproducer (reproducer &r)
{
  r.write (
    "  gcc_jit_global_set_initializer_rvalue (%s, /* lvalue *global */\n"
    "                                         %s);/* rvalue *init */\n",
    r.get_identifier (m_variable),
    r.get_identifier_as_rvalue (m_init));
}

void
recording::global_init_rvalue::write_to_dump (dump &d)
{
  d.write ("%s;\n", get_debug_string ());
}

recording::string *
recording::global_init_rvalue::make_debug_string ()
{
    return string::from_printf (m_ctxt, "%s = %s",
      m_variable->get_debug_string (),
      m_init->get_debug_string ());
}

enum strip_flags {
  STRIP_FLAG_NONE,
  STRIP_FLAG_ARR,
  STRIP_FLAG_VEC
};

/* Strips type down to array, vector or base type (whichever comes first)

   Also saves 'ptr_depth' and sets 'flags' for array or vector types.  */
static
recording::type *
strip_and_count (recording::type *type_to_strip,
		 int &ptr_depth,
		 strip_flags &flags)
{
  recording::type *t = type_to_strip;

  while (true)
    {
      if (!t)
	gcc_unreachable (); /* Should only happen on corrupt input.  */

      recording::type *pointed_to_type = t->is_pointer ();
      if (pointed_to_type != NULL)
	{
	  ptr_depth++;
	  t = pointed_to_type;
	  continue;
	}

      recording::type *array_el = t->is_array ();
      if (array_el != NULL)
	{
	  flags = STRIP_FLAG_ARR;
	  break;
	}

      recording::type *vec = t->dyn_cast_vector_type ();
      if (vec != NULL)
	{
	  flags = STRIP_FLAG_VEC;
	  break;
	}

      /* unqualified () returns 'this' on base types.  */
      recording::type *next = t->unqualified ();
      if (next == t)
	{
	  break;
	}
      t = next;
    }

  return t;
}

/* Strip qualifiers and count pointer depth, returning true
   if the types' base type and pointer depth are
   the same, otherwise false.

   For array and vector types the number of element also
   has to match.

   Do not call this directly.  Call 'types_kinda_same'.  */
bool
types_kinda_same_internal (recording::type *a, recording::type *b)
{
  int ptr_depth_a = 0;
  int ptr_depth_b = 0;
  recording::type *base_a;
  recording::type *base_b;

  strip_flags flags_a = STRIP_FLAG_NONE;
  strip_flags flags_b = STRIP_FLAG_NONE;

  base_a = strip_and_count (a, ptr_depth_a, flags_a);
  base_b = strip_and_count (b, ptr_depth_b, flags_b);

  if (ptr_depth_a != ptr_depth_b)
    return false;

  if (base_a == base_b)
    return true;

  if (flags_a != flags_b)
    return false;

  /* If the "base type" is an array or vector we might need to
     check deeper.  */
  if (flags_a == STRIP_FLAG_ARR)
    {
      recording::array_type *arr_a =
	static_cast<recording::array_type*> (base_a);
      recording::array_type *arr_b =
	static_cast<recording::array_type*> (base_b);

      if (arr_a->num_elements () != arr_b->num_elements ())
	return false;

      /* is_array returns element type.  */
      recording::type *el_a = arr_a->is_array ();
      recording::type *el_b = arr_b->is_array ();

      if (el_a == el_b)
	return true;

      return types_kinda_same_internal (el_a, el_b);
    }
  if (flags_a == STRIP_FLAG_VEC)
    {
      recording::vector_type *arr_a =
	static_cast<recording::vector_type*> (base_a);
      recording::vector_type *arr_b =
	static_cast<recording::vector_type*> (base_b);

      if (arr_a->get_num_units () != arr_b->get_num_units ())
	return false;

      recording::type *el_a = arr_a->get_element_type ();
      recording::type *el_b = arr_b->get_element_type ();

      if (el_a == el_b)
	return true;

      return types_kinda_same_internal (el_a, el_b);
    }

  return false;
}

} // namespace gcc::jit

} // namespace gcc
