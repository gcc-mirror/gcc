/* Implementation of the C API; all wrappers into the internal C++ API
   Copyright (C) 2013-2021 Free Software Foundation, Inc.
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
#include "timevar.h"
#include "typed-splay-tree.h"
#include "cppbuiltin.h"
#include <pthread.h>

#include "libgccjit.h"
#include "jit-recording.h"
#include "jit-result.h"

/* The opaque types used by the public API are actually subclasses
   of the gcc::jit::recording classes.  */

struct gcc_jit_context : public gcc::jit::recording::context
{
  gcc_jit_context (gcc_jit_context *parent_ctxt) :
    context (parent_ctxt)
  {}
};

struct gcc_jit_result : public gcc::jit::result
{
};

struct gcc_jit_object : public gcc::jit::recording::memento
{
};

struct gcc_jit_location : public gcc::jit::recording::location
{
};

struct gcc_jit_type : public gcc::jit::recording::type
{
};

struct gcc_jit_struct : public gcc::jit::recording::struct_
{
};

struct gcc_jit_field : public gcc::jit::recording::field
{
};

struct gcc_jit_bitfield : public gcc::jit::recording::bitfield
{
};

struct gcc_jit_function : public gcc::jit::recording::function
{
};

struct gcc_jit_block : public gcc::jit::recording::block
{
};

struct gcc_jit_rvalue : public gcc::jit::recording::rvalue
{
};

struct gcc_jit_lvalue : public gcc::jit::recording::lvalue
{
};

struct gcc_jit_param : public gcc::jit::recording::param
{
};

struct gcc_jit_case : public gcc::jit::recording::case_
{
};

struct gcc_jit_timer : public timer
{
};

struct gcc_jit_extended_asm : public gcc::jit::recording::extended_asm
{
};


/**********************************************************************
 Error-handling.

 We try to gracefully handle API usage errors by being defensive
 at the API boundary.
 **********************************************************************/

#define JIT_BEGIN_STMT do {
#define JIT_END_STMT   } while(0)

/* Each of these error-handling macros determines if TEST_EXPR holds.

   If TEXT_EXPR fails to hold we return from the enclosing function and
   print an error, either via adding an error on the given context CTXT
   if CTXT is non-NULL, falling back to simply printing to stderr if CTXT
   is NULL.

   They have to be macros since they inject their "return" into the
   function they are placed in.

   The variant macros express:

     (A) whether or not we need to return a value:
	    RETURN_VAL_IF_FAIL* vs
	    RETURN_IF_FAIL*,
	 with the former returning RETURN_EXPR, and
	    RETURN_NULL_IF_FAIL*
	 for the common case where a NULL value is to be returned on
	 error, and

     (B) whether the error message is to be directly printed:
	   RETURN_*IF_FAIL
	 or is a format string with some number of arguments:
	   RETURN_*IF_FAIL_PRINTF*

   They all use JIT_BEGIN_STMT/JIT_END_STMT so they can be written with
   trailing semicolons.
*/

#define RETURN_VAL_IF_FAIL(TEST_EXPR, RETURN_EXPR, CTXT, LOC, ERR_MSG)	\
  JIT_BEGIN_STMT							\
    if (!(TEST_EXPR))							\
      {								\
	jit_error ((CTXT), (LOC), "%s: %s", __func__, (ERR_MSG));	\
	return (RETURN_EXPR);						\
      }								\
  JIT_END_STMT

#define RETURN_VAL_IF_FAIL_PRINTF1(TEST_EXPR, RETURN_EXPR, CTXT, LOC, ERR_FMT, A0) \
  JIT_BEGIN_STMT							\
    if (!(TEST_EXPR))							\
      {								\
	jit_error ((CTXT), (LOC), "%s: " ERR_FMT,			\
		   __func__, (A0));				\
	return (RETURN_EXPR);						\
      }								\
  JIT_END_STMT

#define RETURN_VAL_IF_FAIL_PRINTF2(TEST_EXPR, RETURN_EXPR, CTXT, LOC, ERR_FMT, A0, A1) \
  JIT_BEGIN_STMT							\
    if (!(TEST_EXPR))							\
      {								\
	jit_error ((CTXT), (LOC), "%s: " ERR_FMT,				\
		   __func__, (A0), (A1));				\
	return (RETURN_EXPR);						\
      }								\
  JIT_END_STMT

#define RETURN_VAL_IF_FAIL_PRINTF3(TEST_EXPR, RETURN_EXPR, CTXT, LOC, ERR_FMT, A0, A1, A2) \
  JIT_BEGIN_STMT							\
    if (!(TEST_EXPR))							\
      {								\
	jit_error ((CTXT), (LOC), "%s: " ERR_FMT,				\
		   __func__, (A0), (A1), (A2));			\
	return (RETURN_EXPR);						\
      }								\
  JIT_END_STMT

#define RETURN_VAL_IF_FAIL_PRINTF4(TEST_EXPR, RETURN_EXPR, CTXT, LOC, ERR_FMT, A0, A1, A2, A3) \
  JIT_BEGIN_STMT							\
    if (!(TEST_EXPR))							\
      {								\
	jit_error ((CTXT), (LOC), "%s: " ERR_FMT,				\
		   __func__, (A0), (A1), (A2), (A3));			\
	return (RETURN_EXPR);						\
      }								\
  JIT_END_STMT

#define RETURN_VAL_IF_FAIL_PRINTF5(TEST_EXPR, RETURN_EXPR, CTXT, LOC, ERR_FMT, A0, A1, A2, A3, A4) \
  JIT_BEGIN_STMT							\
    if (!(TEST_EXPR))							\
      {								\
	jit_error ((CTXT), (LOC), "%s: " ERR_FMT,				\
		   __func__, (A0), (A1), (A2), (A3), (A4));	\
	return (RETURN_EXPR);						\
      }								\
  JIT_END_STMT

#define RETURN_VAL_IF_FAIL_PRINTF6(TEST_EXPR, RETURN_EXPR, CTXT, LOC, ERR_FMT, A0, A1, A2, A3, A4, A5) \
  JIT_BEGIN_STMT							\
    if (!(TEST_EXPR))							\
      {								\
	jit_error ((CTXT), (LOC), "%s: " ERR_FMT,				\
		   __func__, (A0), (A1), (A2), (A3), (A4), (A5));	\
	return (RETURN_EXPR);						\
      }								\
  JIT_END_STMT

#define RETURN_NULL_IF_FAIL(TEST_EXPR, CTXT, LOC, ERR_MSG) \
  RETURN_VAL_IF_FAIL ((TEST_EXPR), NULL, (CTXT), (LOC), (ERR_MSG))

#define RETURN_NULL_IF_FAIL_PRINTF1(TEST_EXPR, CTXT, LOC, ERR_FMT, A0) \
  RETURN_VAL_IF_FAIL_PRINTF1 (TEST_EXPR, NULL, CTXT, LOC, ERR_FMT, A0)

#define RETURN_NULL_IF_FAIL_PRINTF2(TEST_EXPR, CTXT, LOC, ERR_FMT, A0, A1) \
  RETURN_VAL_IF_FAIL_PRINTF2 (TEST_EXPR, NULL, CTXT, LOC, ERR_FMT, A0, A1)

#define RETURN_NULL_IF_FAIL_PRINTF3(TEST_EXPR, CTXT, LOC, ERR_FMT, A0, A1, A2) \
  RETURN_VAL_IF_FAIL_PRINTF3 (TEST_EXPR, NULL, CTXT, LOC, ERR_FMT, A0, A1, A2)

#define RETURN_NULL_IF_FAIL_PRINTF4(TEST_EXPR, CTXT, LOC, ERR_FMT, A0, A1, A2, A3) \
  RETURN_VAL_IF_FAIL_PRINTF4 (TEST_EXPR, NULL, CTXT, LOC, ERR_FMT, A0, A1, A2, A3)

#define RETURN_NULL_IF_FAIL_PRINTF5(TEST_EXPR, CTXT, LOC, ERR_FMT, A0, A1, A2, A3, A4) \
  RETURN_VAL_IF_FAIL_PRINTF5 (TEST_EXPR, NULL, CTXT, LOC, ERR_FMT, A0, A1, A2, A3, A4)

#define RETURN_NULL_IF_FAIL_PRINTF6(TEST_EXPR, CTXT, LOC, ERR_FMT, A0, A1, A2, A3, A4, A5) \
  RETURN_VAL_IF_FAIL_PRINTF6 (TEST_EXPR, NULL, CTXT, LOC, ERR_FMT, A0, A1, A2, A3, A4, A5)

#define RETURN_IF_FAIL(TEST_EXPR, CTXT, LOC, ERR_MSG)			\
  JIT_BEGIN_STMT							\
    if (!(TEST_EXPR))							\
      {								\
	jit_error ((CTXT), (LOC), "%s: %s", __func__, (ERR_MSG));		\
	return;							\
      }								\
  JIT_END_STMT

#define RETURN_IF_FAIL_PRINTF1(TEST_EXPR, CTXT, LOC, ERR_FMT, A0) \
  JIT_BEGIN_STMT							\
    if (!(TEST_EXPR))							\
      {								\
	jit_error ((CTXT), (LOC), "%s: " ERR_FMT,				\
		   __func__, (A0));					\
	return;							\
      }								\
  JIT_END_STMT

#define RETURN_IF_FAIL_PRINTF2(TEST_EXPR, CTXT, LOC, ERR_FMT, A0, A1) \
  JIT_BEGIN_STMT							\
    if (!(TEST_EXPR))							\
      {								\
	jit_error ((CTXT), (LOC), "%s: " ERR_FMT,				\
		   __func__, (A0), (A1));				\
	return;							\
      }								\
  JIT_END_STMT

#define RETURN_IF_FAIL_PRINTF4(TEST_EXPR, CTXT, LOC, ERR_FMT, A0, A1, A2, A3) \
  JIT_BEGIN_STMT							\
    if (!(TEST_EXPR))							\
      {								\
	jit_error ((CTXT), (LOC), "%s: " ERR_FMT,				\
		   __func__, (A0), (A1), (A2), (A3));			\
	return;							\
      }								\
  JIT_END_STMT

/* Check that BLOCK is non-NULL, and that it's OK to add statements to
   it.  This will fail if BLOCK has already been terminated by some
   kind of jump or a return.  */
#define RETURN_IF_NOT_VALID_BLOCK(BLOCK, LOC)				\
  JIT_BEGIN_STMT							\
    RETURN_IF_FAIL ((BLOCK), NULL, (LOC), "NULL block");		\
    RETURN_IF_FAIL_PRINTF2 (						\
      !(BLOCK)->has_been_terminated (),				\
      (BLOCK)->get_context (),						\
      (LOC),								\
      "adding to terminated block: %s (already terminated by: %s)",	\
      (BLOCK)->get_debug_string (),					\
      (BLOCK)->get_last_statement ()->get_debug_string ());		\
  JIT_END_STMT

/* As RETURN_IF_NOT_VALID_BLOCK, but injecting a "return NULL;" if it
   fails.  */
#define RETURN_NULL_IF_NOT_VALID_BLOCK(BLOCK, LOC)			\
  JIT_BEGIN_STMT							\
    RETURN_NULL_IF_FAIL ((BLOCK), NULL, (LOC), "NULL block");		\
    RETURN_NULL_IF_FAIL_PRINTF2 (					\
      !(BLOCK)->has_been_terminated (),				\
      (BLOCK)->get_context (),						\
      (LOC),								\
      "adding to terminated block: %s (already terminated by: %s)",	\
      (BLOCK)->get_debug_string (),					\
      (BLOCK)->get_last_statement ()->get_debug_string ());		\
  JIT_END_STMT

/* Format the given string, and report it as an error, either on CTXT
   if non-NULL, or by printing to stderr if we have a NULL context.
   LOC gives the source location where the error occcurred, and can be
   NULL.  */

static void
jit_error (gcc::jit::recording::context *ctxt,
	   gcc::jit::recording::location *loc,
	   const char *fmt, ...)
  GNU_PRINTF(3, 4);

static void
jit_error (gcc::jit::recording::context *ctxt,
	   gcc::jit::recording::location *loc,
	   const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);

  if (ctxt)
    ctxt->add_error_va (loc, fmt, ap);
  else
    {
      /* No context?  Send to stderr.  */
      vfprintf (stderr, fmt, ap);
      fprintf (stderr, "\n");
    }

  va_end (ap);
}

/* Determine whether or not we can write to lvalues of type LTYPE from
   rvalues of type RTYPE, detecting type errors such as attempting to
   write to an int with a string literal (without an explicit cast).

   This is implemented by calling the
   gcc::jit::recording::type::accepts_writes_from virtual function on
   LTYPE.  */

static bool
compatible_types (gcc::jit::recording::type *ltype,
		  gcc::jit::recording::type *rtype)
{
  return ltype->accepts_writes_from (rtype);
}

/* Public entrypoint for acquiring a gcc_jit_context.
   Note that this creates a new top-level context; contrast with
   gcc_jit_context_new_child_context below.

   The real work is done in the constructor for
   gcc::jit::recording::context in jit-recording.c. */

gcc_jit_context *
gcc_jit_context_acquire (void)
{
  gcc_jit_context *ctxt = new gcc_jit_context (NULL);
  ctxt->log ("new top-level ctxt: %p", (void *)ctxt);
  return ctxt;
}

/* Public entrypoint for releasing a gcc_jit_context.
   The real work is done in the destructor for
   gcc::jit::recording::context in jit-recording.c.  */

void
gcc_jit_context_release (gcc_jit_context *ctxt)
{
  RETURN_IF_FAIL (ctxt, NULL, NULL, "NULL ctxt");
  JIT_LOG_FUNC (ctxt->get_logger ());
  ctxt->log ("deleting ctxt: %p", (void *)ctxt);
  delete ctxt;
}

/* Public entrypoint for creating a child context within
   PARENT_CTXT.  See description in libgccjit.h.

   The real work is done in the constructor for
   gcc::jit::recording::context in jit-recording.c. */

gcc_jit_context *
gcc_jit_context_new_child_context (gcc_jit_context *parent_ctxt)
{
  RETURN_NULL_IF_FAIL (parent_ctxt, NULL, NULL, "NULL parent ctxt");
  JIT_LOG_FUNC (parent_ctxt->get_logger ());
  parent_ctxt->log ("parent_ctxt: %p", (void *)parent_ctxt);
  gcc_jit_context *child_ctxt = new gcc_jit_context (parent_ctxt);
  child_ctxt->log ("new child_ctxt: %p", (void *)child_ctxt);
  return child_ctxt;
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
     gcc::jit::recording::context::new_location
   method in jit-recording.c.  */

gcc_jit_location *
gcc_jit_context_new_location (gcc_jit_context *ctxt,
			      const char *filename,
			      int line,
			      int column)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  return (gcc_jit_location *)ctxt->new_location (filename, line, column, true);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, this calls the trivial
   gcc::jit::recording::memento::as_object method (a location is a
   memento), in jit-recording.h.  */

gcc_jit_object *
gcc_jit_location_as_object (gcc_jit_location *loc)
{
  RETURN_NULL_IF_FAIL (loc, NULL, NULL, "NULL location");

  return static_cast <gcc_jit_object *> (loc->as_object ());
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, this calls the trivial
   gcc::jit::recording::memento::as_object method (a type is a
   memento), in jit-recording.h.  */

gcc_jit_object *
gcc_jit_type_as_object (gcc_jit_type *type)
{
  RETURN_NULL_IF_FAIL (type, NULL, NULL, "NULL type");

  return static_cast <gcc_jit_object *> (type->as_object ());
}

/* Public entrypoint for getting a specific type from a context.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::get_type method, in
   jit-recording.c  */

gcc_jit_type *
gcc_jit_context_get_type (gcc_jit_context *ctxt,
			  enum gcc_jit_types type)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_NULL_IF_FAIL_PRINTF1 (
    (type >= GCC_JIT_TYPE_VOID
     && type <= GCC_JIT_TYPE_FILE_PTR),
    ctxt, NULL,
    "unrecognized value for enum gcc_jit_types: %i", type);

  return (gcc_jit_type *)ctxt->get_type (type);
}

/* Public entrypoint for getting the integer type of the given size and
   signedness.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::get_int_type method,
   in jit-recording.c.  */

gcc_jit_type *
gcc_jit_context_get_int_type (gcc_jit_context *ctxt,
			      int num_bytes, int is_signed)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_NULL_IF_FAIL (num_bytes >= 0, ctxt, NULL, "negative size");

  return (gcc_jit_type *)ctxt->get_int_type (num_bytes, is_signed);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::type::get_pointer method, in
   jit-recording.c  */

gcc_jit_type *
gcc_jit_type_get_pointer (gcc_jit_type *type)
{
  RETURN_NULL_IF_FAIL (type, NULL, NULL, "NULL type");

  return (gcc_jit_type *)type->get_pointer ();
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::type::get_const method, in
   jit-recording.c.  */

gcc_jit_type *
gcc_jit_type_get_const (gcc_jit_type *type)
{
  RETURN_NULL_IF_FAIL (type, NULL, NULL, "NULL type");

  return (gcc_jit_type *)type->get_const ();
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::type::get_volatile method, in
   jit-recording.c.  */

gcc_jit_type *
gcc_jit_type_get_volatile (gcc_jit_type *type)
{
  RETURN_NULL_IF_FAIL (type, NULL, NULL, "NULL type");

  return (gcc_jit_type *)type->get_volatile ();
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_array_type method, in
   jit-recording.c.  */

gcc_jit_type *
gcc_jit_context_new_array_type (gcc_jit_context *ctxt,
				gcc_jit_location *loc,
				gcc_jit_type *element_type,
				int num_elements)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, loc, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (element_type, ctxt, loc, "NULL type");
  RETURN_NULL_IF_FAIL (num_elements >= 0, ctxt, NULL, "negative size");
  RETURN_NULL_IF_FAIL (!element_type->is_void (), ctxt, loc,
		       "void type for elements");

  return (gcc_jit_type *)ctxt->new_array_type (loc,
					       element_type,
					       num_elements);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_field method, in
   jit-recording.c.  */

gcc_jit_field *
gcc_jit_context_new_field (gcc_jit_context *ctxt,
			   gcc_jit_location *loc,
			   gcc_jit_type *type,
			   const char *name)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (type, ctxt, loc, "NULL type");
  RETURN_NULL_IF_FAIL (name, ctxt, loc, "NULL name");
  RETURN_NULL_IF_FAIL_PRINTF2 (
    type->has_known_size (),
    ctxt, loc,
    "unknown size for field \"%s\" (type: %s)",
    name,
    type->get_debug_string ());
  RETURN_NULL_IF_FAIL_PRINTF1 (
    !type->is_void (),
    ctxt, loc,
    "void type for field \"%s\"",
    name);

  return (gcc_jit_field *)ctxt->new_field (loc, type, name);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_bitfield method, in
   jit-recording.c.  */

gcc_jit_field *
gcc_jit_context_new_bitfield (gcc_jit_context *ctxt,
			      gcc_jit_location *loc,
			      gcc_jit_type *type,
			      int width,
			      const char *name)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (name, ctxt, loc, "NULL name");
  RETURN_NULL_IF_FAIL (type, ctxt, loc, "NULL type");
  RETURN_NULL_IF_FAIL_PRINTF2 (type->is_int () || type->is_bool (),
			       ctxt, loc,
			       "bit-field %s has non integral type %s",
			       name, type->get_debug_string ());
  RETURN_NULL_IF_FAIL_PRINTF2 (
    width > 0, ctxt, loc,
    "invalid width %d for bitfield \"%s\" (must be > 0)",
    width, name);
  RETURN_NULL_IF_FAIL_PRINTF2 (
    type->has_known_size (),
    ctxt, loc,
    "unknown size for field \"%s\" (type: %s)",
    name,
    type->get_debug_string ());

  return (gcc_jit_field *)ctxt->new_bitfield (loc, type, width, name);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, this calls the trivial
   gcc::jit::recording::memento::as_object method (a field is a
   memento), in jit-recording.h.  */

gcc_jit_object *
gcc_jit_field_as_object (gcc_jit_field *field)
{
  RETURN_NULL_IF_FAIL (field, NULL, NULL, "NULL field");

  return static_cast <gcc_jit_object *> (field->as_object ());
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_struct_type method,
   immediately followed by a "set_fields" call on the resulting
   gcc::jit::recording::compound_type *, both in jit-recording.c  */

gcc_jit_struct *
gcc_jit_context_new_struct_type (gcc_jit_context *ctxt,
				 gcc_jit_location *loc,
				 const char *name,
				 int num_fields,
				 gcc_jit_field **fields)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (name, ctxt, loc, "NULL name");
  if (num_fields)
    RETURN_NULL_IF_FAIL (fields, ctxt, loc, "NULL fields ptr");
  for (int i = 0; i < num_fields; i++)
    {
      RETURN_NULL_IF_FAIL (fields[i], ctxt, loc, "NULL field ptr");
      RETURN_NULL_IF_FAIL_PRINTF2 (
	fields[i]->get_container () == NULL,
	ctxt, loc,
	"%s is already a field of %s",
	fields[i]->get_debug_string (),
	fields[i]->get_container ()->get_debug_string ());
    }

  gcc::jit::recording::struct_ *result =
    ctxt->new_struct_type (loc, name);
  result->set_fields (loc,
		      num_fields,
		      (gcc::jit::recording::field **)fields);
  return static_cast<gcc_jit_struct *> (result);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_struct_type method in
   jit-recording.c.  */

gcc_jit_struct *
gcc_jit_context_new_opaque_struct (gcc_jit_context *ctxt,
				   gcc_jit_location *loc,
				   const char *name)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, loc, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (name, ctxt, loc, "NULL name");

  return (gcc_jit_struct *)ctxt->new_struct_type (loc, name);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, this calls the trivial
   gcc::jit::recording::struct_::as_object method in
   jit-recording.h.  */

gcc_jit_type *
gcc_jit_struct_as_type (gcc_jit_struct *struct_type)
{
  RETURN_NULL_IF_FAIL (struct_type, NULL, NULL, "NULL struct_type");

  return static_cast <gcc_jit_type *> (struct_type->as_type ());
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::compound_type::set_fields method in
   jit-recording.c.  */

void
gcc_jit_struct_set_fields (gcc_jit_struct *struct_type,
			   gcc_jit_location *loc,
			   int num_fields,
			   gcc_jit_field **fields)
{
  RETURN_IF_FAIL (struct_type, NULL, loc, "NULL struct_type");
  gcc::jit::recording::context *ctxt = struct_type->m_ctxt;
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_IF_FAIL_PRINTF1 (
    struct_type->get_fields () == NULL, ctxt, loc,
    "%s already has had fields set",
    struct_type->get_debug_string ());
  if (num_fields)
    RETURN_IF_FAIL (fields, ctxt, loc, "NULL fields ptr");
  for (int i = 0; i < num_fields; i++)
    {
      RETURN_IF_FAIL_PRINTF2 (
	fields[i],
	ctxt, loc,
	"%s: NULL field ptr at index %i",
	struct_type->get_debug_string (),
	i);
      RETURN_IF_FAIL_PRINTF2 (
	fields[i]->get_container () == NULL,
	ctxt, loc,
	"%s is already a field of %s",
	fields[i]->get_debug_string (),
	fields[i]->get_container ()->get_debug_string ());
    }

  struct_type->set_fields (loc, num_fields,
			   (gcc::jit::recording::field **)fields);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_union_type method,
   immediately followed by a "set_fields" call on the resulting
   gcc::jit::recording::compound_type *, both in jit-recording.c  */

gcc_jit_type *
gcc_jit_context_new_union_type (gcc_jit_context *ctxt,
				gcc_jit_location *loc,
				const char *name,
				int num_fields,
				gcc_jit_field **fields)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (name, ctxt, loc, "NULL name");
  if (num_fields)
    RETURN_NULL_IF_FAIL (fields, ctxt, loc, "NULL fields ptr");
  for (int i = 0; i < num_fields; i++)
    {
      RETURN_NULL_IF_FAIL (fields[i], ctxt, loc, "NULL field ptr");
      RETURN_NULL_IF_FAIL_PRINTF2 (
	fields[i]->get_container () == NULL,
	ctxt, loc,
	"%s is already a field of %s",
	fields[i]->get_debug_string (),
	fields[i]->get_container ()->get_debug_string ());
    }

  gcc::jit::recording::union_ *result =
    ctxt->new_union_type (loc, name);
  result->set_fields (loc,
		      num_fields,
		      (gcc::jit::recording::field **)fields);
  return (gcc_jit_type *) (result);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_function_ptr_type method,
   in jit-recording.c  */

gcc_jit_type *
gcc_jit_context_new_function_ptr_type (gcc_jit_context *ctxt,
				       gcc_jit_location *loc,
				       gcc_jit_type *return_type,
				       int num_params,
				       gcc_jit_type **param_types,
				       int is_variadic)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, loc, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (return_type, ctxt, loc, "NULL return_type");
  RETURN_NULL_IF_FAIL (
    (num_params == 0) || param_types,
    ctxt, loc,
    "NULL param_types creating function pointer type");
  for (int i = 0; i < num_params; i++)
    {
      RETURN_NULL_IF_FAIL_PRINTF1 (param_types[i],
				   ctxt, loc,
				   "NULL parameter type %i"
				   " creating function pointer type", i);
      RETURN_NULL_IF_FAIL_PRINTF1 (!param_types[i]->is_void (),
				   ctxt, loc,
				   "void type for param %i", i);
    }

  return (gcc_jit_type*)
    ctxt->new_function_ptr_type (loc, return_type,
				 num_params,
				 (gcc::jit::recording::type **)param_types,
				 is_variadic);
}

/* Constructing functions.  */

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_param method, in jit-recording.c  */

gcc_jit_param *
gcc_jit_context_new_param (gcc_jit_context *ctxt,
			   gcc_jit_location *loc,
			   gcc_jit_type *type,
			   const char *name)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, loc, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (type, ctxt, loc, "NULL type");
  RETURN_NULL_IF_FAIL (name, ctxt, loc, "NULL name");
  RETURN_NULL_IF_FAIL_PRINTF1 (!type->is_void (),
			       ctxt, loc,
			       "void type for param \"%s\"", name);

  return (gcc_jit_param *)ctxt->new_param (loc, type, name);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, this calls the trivial
   gcc::jit::recording::memento::as_object method (a param is a memento),
   in jit-recording.h.  */

gcc_jit_object *
gcc_jit_param_as_object (gcc_jit_param *param)
{
  RETURN_NULL_IF_FAIL (param, NULL, NULL, "NULL param");

  return static_cast <gcc_jit_object *> (param->as_object ());
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, this calls the trivial
   gcc::jit::recording::param::as_lvalue method in jit-recording.h.  */

gcc_jit_lvalue *
gcc_jit_param_as_lvalue (gcc_jit_param *param)
{
  RETURN_NULL_IF_FAIL (param, NULL, NULL, "NULL param");

  return (gcc_jit_lvalue *)param->as_lvalue ();
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, this calls the trivial
   gcc::jit::recording::lvalue::as_rvalue method (a param is an rvalue),
   in jit-recording.h.  */

gcc_jit_rvalue *
gcc_jit_param_as_rvalue (gcc_jit_param *param)
{
  RETURN_NULL_IF_FAIL (param, NULL, NULL, "NULL param");

  return (gcc_jit_rvalue *)param->as_rvalue ();
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_function method, in
   jit-recording.c.  */

gcc_jit_function *
gcc_jit_context_new_function (gcc_jit_context *ctxt,
			      gcc_jit_location *loc,
			      enum gcc_jit_function_kind kind,
			      gcc_jit_type *return_type,
			      const char *name,
			      int num_params,
			      gcc_jit_param **params,
			      int is_variadic)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, loc, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL_PRINTF1 (
    ((kind >= GCC_JIT_FUNCTION_EXPORTED)
     && (kind <= GCC_JIT_FUNCTION_ALWAYS_INLINE)),
    ctxt, loc,
    "unrecognized value for enum gcc_jit_function_kind: %i",
    kind);
  RETURN_NULL_IF_FAIL (return_type, ctxt, loc, "NULL return_type");
  RETURN_NULL_IF_FAIL (name, ctxt, loc, "NULL name");
  /* The assembler can only handle certain names, so for now, enforce
     C's rules for identifiers upon the name, using ISALPHA and ISALNUM
     from safe-ctype.h to ignore the current locale.
     Eventually we'll need some way to interact with e.g. C++ name
     mangling.  */
  {
    /* Leading char: */
    char ch = *name;
    RETURN_NULL_IF_FAIL_PRINTF2 (
	ISALPHA (ch) || ch == '_',
	ctxt, loc,
	"name \"%s\" contains invalid character: '%c'",
	name, ch);
    /* Subsequent chars: */
    for (const char *ptr = name + 1; (ch = *ptr); ptr++)
      {
	RETURN_NULL_IF_FAIL_PRINTF2 (
	  ISALNUM (ch) || ch == '_',
	  ctxt, loc,
	  "name \"%s\" contains invalid character: '%c'",
	  name, ch);
      }
  }
  RETURN_NULL_IF_FAIL_PRINTF1 (
    (num_params == 0) || params,
    ctxt, loc,
    "NULL params creating function %s", name);
  for (int i = 0; i < num_params; i++)
    {
      RETURN_NULL_IF_FAIL_PRINTF2 (
	params[i],
	ctxt, loc,
	"NULL parameter %i creating function %s", i, name);
      RETURN_NULL_IF_FAIL_PRINTF5 (
	params[i]->get_scope () == NULL,
	ctxt, loc,
	"parameter %i \"%s\""
	" (type: %s)"
	" for function %s"
	" was already used for function %s",
	i, params[i]->get_debug_string (),
	params[i]->get_type ()->get_debug_string (),
	name,
	params[i]->get_scope ()->get_debug_string ());
    }

  return (gcc_jit_function*)
    ctxt->new_function (loc, kind, return_type, name,
			num_params,
			(gcc::jit::recording::param **)params,
			is_variadic,
			BUILT_IN_NONE);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::get_builtin_function method, in
   jit-recording.c.  */

gcc_jit_function *
gcc_jit_context_get_builtin_function (gcc_jit_context *ctxt,
				      const char *name)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_NULL_IF_FAIL (name, ctxt, NULL, "NULL name");

  return static_cast <gcc_jit_function *> (ctxt->get_builtin_function (name));
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, this calls the trivial
   gcc::jit::recording::memento::as_object method (a function is a
   memento), in jit-recording.h.  */

gcc_jit_object *
gcc_jit_function_as_object (gcc_jit_function *func)
{
  RETURN_NULL_IF_FAIL (func, NULL, NULL, "NULL function");

  return static_cast <gcc_jit_object *> (func->as_object ());
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::function::get_param method, in
   jit-recording.h.  */

gcc_jit_param *
gcc_jit_function_get_param (gcc_jit_function *func, int index)
{
  RETURN_NULL_IF_FAIL (func, NULL, NULL, "NULL function");
  gcc::jit::recording::context *ctxt = func->m_ctxt;
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_NULL_IF_FAIL (index >= 0, ctxt, NULL, "negative index");
  int num_params = func->get_params ().length ();
  RETURN_NULL_IF_FAIL_PRINTF3 (index < num_params,
			       ctxt, NULL,
			       "index of %d is too large (%s has %d params)",
			       index,
			       func->get_debug_string (),
			       num_params);

  return static_cast <gcc_jit_param *> (func->get_param (index));
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::function::dump_to_dot method, in
   jit-recording.c.  */

void
gcc_jit_function_dump_to_dot (gcc_jit_function *func,
			      const char *path)
{
  RETURN_IF_FAIL (func, NULL, NULL, "NULL function");
  gcc::jit::recording::context *ctxt = func->m_ctxt;
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_IF_FAIL (path, ctxt, NULL, "NULL path");

  func->dump_to_dot (path);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::function::new_block method, in
   jit-recording.c.  */

gcc_jit_block*
gcc_jit_function_new_block (gcc_jit_function *func,
			    const char *name)
{
  RETURN_NULL_IF_FAIL (func, NULL, NULL, "NULL function");
  JIT_LOG_FUNC (func->get_context ()->get_logger ());
  RETURN_NULL_IF_FAIL (func->get_kind () != GCC_JIT_FUNCTION_IMPORTED,
		       func->get_context (), NULL,
		       "cannot add block to an imported function");
  /* name can be NULL.  */

  return (gcc_jit_block *)func->new_block (name);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, this calls the trivial
   gcc::jit::recording::memento::as_object method (a block is a
   memento), in jit-recording.h.  */

gcc_jit_object *
gcc_jit_block_as_object (gcc_jit_block *block)
{
  RETURN_NULL_IF_FAIL (block, NULL, NULL, "NULL block");

  return static_cast <gcc_jit_object *> (block->as_object ());
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::block::get_function method, in
   jit-recording.h.  */

gcc_jit_function *
gcc_jit_block_get_function (gcc_jit_block *block)
{
  RETURN_NULL_IF_FAIL (block, NULL, NULL, "NULL block");

  return static_cast <gcc_jit_function *> (block->get_function ());
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_global method, in
   jit-recording.c.  */

gcc_jit_lvalue *
gcc_jit_context_new_global (gcc_jit_context *ctxt,
			    gcc_jit_location *loc,
			    enum gcc_jit_global_kind kind,
			    gcc_jit_type *type,
			    const char *name)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, loc, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL_PRINTF1 (
    ((kind >= GCC_JIT_GLOBAL_EXPORTED)
     && (kind <= GCC_JIT_GLOBAL_IMPORTED)),
    ctxt, loc,
    "unrecognized value for enum gcc_jit_global_kind: %i",
    kind);
  RETURN_NULL_IF_FAIL (type, ctxt, loc, "NULL type");
  RETURN_NULL_IF_FAIL (name, ctxt, loc, "NULL name");
  RETURN_NULL_IF_FAIL_PRINTF2 (
    type->has_known_size (),
    ctxt, loc,
    "unknown size for global \"%s\" (type: %s)",
    name,
    type->get_debug_string ());
  RETURN_NULL_IF_FAIL_PRINTF1 (
    !type->is_void (),
    ctxt, loc,
    "void type for global \"%s\"",
    name);

  return (gcc_jit_lvalue *)ctxt->new_global (loc, kind, type, name);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::global::set_initializer method, in
   jit-recording.c.  */

extern gcc_jit_lvalue *
gcc_jit_global_set_initializer (gcc_jit_lvalue *global,
				const void *blob,
				size_t num_bytes)
{
  RETURN_NULL_IF_FAIL (global, NULL, NULL, "NULL global");
  RETURN_NULL_IF_FAIL (blob, NULL, NULL, "NULL blob");
  RETURN_NULL_IF_FAIL_PRINTF1 (global->is_global (), NULL, NULL,
			       "lvalue \"%s\" not a global",
			       global->get_debug_string ());

  gcc::jit::recording::type *lval_type = global->get_type ();
  RETURN_NULL_IF_FAIL_PRINTF1 (lval_type->is_array (), NULL, NULL,
			       "global \"%s\" is not an array",
			       global->get_debug_string ());
  RETURN_NULL_IF_FAIL_PRINTF1 (lval_type->dereference ()->is_int (), NULL, NULL,
			       "global \"%s\" is not an array of integral type",
			       global->get_debug_string ());
  size_t lvalue_size =
    lval_type->dereference ()->get_size ()
    * static_cast <gcc::jit::recording::array_type *> (lval_type)->num_elements ();
  RETURN_NULL_IF_FAIL_PRINTF3 (
    lvalue_size == num_bytes, NULL, NULL,
    "mismatching sizes:"
    " global \"%s\" has size %zu whereas initializer has size %zu",
    global->get_debug_string (), lvalue_size, num_bytes);

  reinterpret_cast <gcc::jit::recording::global *> (global)
    ->set_initializer (blob, num_bytes);

  return global;
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, this calls the trivial
   gcc::jit::recording::memento::as_object method (an lvalue is a
   memento), in jit-recording.h.  */

gcc_jit_object *
gcc_jit_lvalue_as_object (gcc_jit_lvalue *lvalue)
{
  RETURN_NULL_IF_FAIL (lvalue, NULL, NULL, "NULL lvalue");

  return static_cast <gcc_jit_object *> (lvalue->as_object ());
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, this calls the trivial
   gcc::jit::recording::lvalue::as_rvalue method in jit-recording.h.  */

gcc_jit_rvalue *
gcc_jit_lvalue_as_rvalue (gcc_jit_lvalue *lvalue)
{
  RETURN_NULL_IF_FAIL (lvalue, NULL, NULL, "NULL lvalue");

  return (gcc_jit_rvalue *)lvalue->as_rvalue ();
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, this calls the trivial
   gcc::jit::recording::memento::as_object method (an rvalue is a
   memento), in jit-recording.h.  */

gcc_jit_object *
gcc_jit_rvalue_as_object (gcc_jit_rvalue *rvalue)
{
  RETURN_NULL_IF_FAIL (rvalue, NULL, NULL, "NULL rvalue");

  return static_cast <gcc_jit_object *> (rvalue->as_object ());
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::rvalue::get_type method, in
   jit-recording.h.  */

gcc_jit_type *
gcc_jit_rvalue_get_type (gcc_jit_rvalue *rvalue)
{
  RETURN_NULL_IF_FAIL (rvalue, NULL, NULL, "NULL rvalue");

  return static_cast <gcc_jit_type *> (rvalue->get_type ());
}

/* Verify that NUMERIC_TYPE is non-NULL, and that it is a "numeric"
   type i.e. it satisfies gcc::jit::type::is_numeric (), such as the
   result of gcc_jit_context_get_type (GCC_JIT_TYPE_INT).  */

#define RETURN_NULL_IF_FAIL_NONNULL_NUMERIC_TYPE(CTXT, NUMERIC_TYPE) \
  JIT_BEGIN_STMT						     \
  RETURN_NULL_IF_FAIL (NUMERIC_TYPE, CTXT, NULL, "NULL type"); \
  RETURN_NULL_IF_FAIL_PRINTF1 (                                \
    NUMERIC_TYPE->is_numeric (), ctxt, NULL,                   \
    "not a numeric type: %s",                                  \
    NUMERIC_TYPE->get_debug_string ()); \
  JIT_END_STMT

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_rvalue_from_const <int> method in
   jit-recording.c.  */

gcc_jit_rvalue *
gcc_jit_context_new_rvalue_from_int (gcc_jit_context *ctxt,
				     gcc_jit_type *numeric_type,
				     int value)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_NULL_IF_FAIL_NONNULL_NUMERIC_TYPE (ctxt, numeric_type);

  return ((gcc_jit_rvalue *)ctxt
	  ->new_rvalue_from_const <int> (numeric_type, value));
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_rvalue_from_const <long> method
   in jit-recording.c.  */

gcc_jit_rvalue *
gcc_jit_context_new_rvalue_from_long (gcc_jit_context *ctxt,
				      gcc_jit_type *numeric_type,
				      long value)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_NULL_IF_FAIL_NONNULL_NUMERIC_TYPE (ctxt, numeric_type);

  return ((gcc_jit_rvalue *)ctxt
	  ->new_rvalue_from_const <long> (numeric_type, value));
}

/* Public entrypoint.  See description in libgccjit.h.

   This is essentially equivalent to:
      gcc_jit_context_new_rvalue_from_int (ctxt, numeric_type, 0);
   albeit with slightly different error messages if an error occurs.  */

gcc_jit_rvalue *
gcc_jit_context_zero (gcc_jit_context *ctxt,
		      gcc_jit_type *numeric_type)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_NULL_IF_FAIL_NONNULL_NUMERIC_TYPE (ctxt, numeric_type);

  return gcc_jit_context_new_rvalue_from_int (ctxt, numeric_type, 0);
}

/* Public entrypoint.  See description in libgccjit.h.

   This is essentially equivalent to:
      gcc_jit_context_new_rvalue_from_int (ctxt, numeric_type, 1);
   albeit with slightly different error messages if an error occurs.  */

gcc_jit_rvalue *
gcc_jit_context_one (gcc_jit_context *ctxt,
		     gcc_jit_type *numeric_type)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_NULL_IF_FAIL_NONNULL_NUMERIC_TYPE (ctxt, numeric_type);

  return gcc_jit_context_new_rvalue_from_int (ctxt, numeric_type, 1);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_rvalue_from_const <double> method in
   jit-recording.c.  */

gcc_jit_rvalue *
gcc_jit_context_new_rvalue_from_double (gcc_jit_context *ctxt,
					gcc_jit_type *numeric_type,
					double value)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_NULL_IF_FAIL_NONNULL_NUMERIC_TYPE (ctxt, numeric_type);

  return ((gcc_jit_rvalue *)ctxt
	  ->new_rvalue_from_const <double> (numeric_type, value));
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_rvalue_from_const <void *> method
   in jit-recording.c.  */

gcc_jit_rvalue *
gcc_jit_context_new_rvalue_from_ptr (gcc_jit_context *ctxt,
				     gcc_jit_type *pointer_type,
				     void *value)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_NULL_IF_FAIL (pointer_type, ctxt, NULL, "NULL type");
  RETURN_NULL_IF_FAIL_PRINTF1 (
    pointer_type->is_pointer (),
    ctxt, NULL,
    "not a pointer type (type: %s)",
    pointer_type->get_debug_string ());

  return ((gcc_jit_rvalue *)ctxt
	  ->new_rvalue_from_const <void *> (pointer_type, value));
}

/* Public entrypoint.  See description in libgccjit.h.

   This is essentially equivalent to:
      gcc_jit_context_new_rvalue_from_ptr (ctxt, pointer_type, NULL);
   albeit with slightly different error messages if an error occurs.  */

gcc_jit_rvalue *
gcc_jit_context_null (gcc_jit_context *ctxt,
		      gcc_jit_type *pointer_type)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_NULL_IF_FAIL (pointer_type, ctxt, NULL, "NULL type");
  RETURN_NULL_IF_FAIL_PRINTF1 (
    pointer_type->is_pointer (),
    ctxt, NULL,
    "not a pointer type (type: %s)",
    pointer_type->get_debug_string ());

  return gcc_jit_context_new_rvalue_from_ptr (ctxt, pointer_type, NULL);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_string_literal method in
   jit-recording.c.  */

gcc_jit_rvalue *
gcc_jit_context_new_string_literal (gcc_jit_context *ctxt,
				    const char *value)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_NULL_IF_FAIL (value, ctxt, NULL, "NULL value");

  return (gcc_jit_rvalue *)ctxt->new_string_literal (value);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_unary_op method in
   jit-recording.c.  */

gcc_jit_rvalue *
gcc_jit_context_new_unary_op (gcc_jit_context *ctxt,
			      gcc_jit_location *loc,
			      enum gcc_jit_unary_op op,
			      gcc_jit_type *result_type,
			      gcc_jit_rvalue *rvalue)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, loc, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL_PRINTF1 (
    (op >= GCC_JIT_UNARY_OP_MINUS
     && op <= GCC_JIT_UNARY_OP_ABS),
    ctxt, loc,
    "unrecognized value for enum gcc_jit_unary_op: %i",
    op);
  RETURN_NULL_IF_FAIL (result_type, ctxt, loc, "NULL result_type");
  RETURN_NULL_IF_FAIL_PRINTF3 (
    result_type->is_numeric (), ctxt, loc,
    "gcc_jit_unary_op %s with operand %s "
    "has non-numeric result_type: %s",
    gcc::jit::unary_op_reproducer_strings[op],
    rvalue->get_debug_string (),
    result_type->get_debug_string ());
  RETURN_NULL_IF_FAIL (rvalue, ctxt, loc, "NULL rvalue");

  return (gcc_jit_rvalue *)ctxt->new_unary_op (loc, op, result_type, rvalue);
}

/* Determine if OP is a valid value for enum gcc_jit_binary_op.
   For use by both gcc_jit_context_new_binary_op and
   gcc_jit_block_add_assignment_op.  */

static bool
valid_binary_op_p (enum gcc_jit_binary_op op)
{
  return (op >= GCC_JIT_BINARY_OP_PLUS
	  && op <= GCC_JIT_BINARY_OP_RSHIFT);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_binary_op method in
   jit-recording.c.  */

gcc_jit_rvalue *
gcc_jit_context_new_binary_op (gcc_jit_context *ctxt,
			       gcc_jit_location *loc,
			       enum gcc_jit_binary_op op,
			       gcc_jit_type *result_type,
			       gcc_jit_rvalue *a, gcc_jit_rvalue *b)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, loc, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL_PRINTF1 (
    valid_binary_op_p (op),
    ctxt, loc,
    "unrecognized value for enum gcc_jit_binary_op: %i",
    op);
  RETURN_NULL_IF_FAIL (result_type, ctxt, loc, "NULL result_type");
  RETURN_NULL_IF_FAIL (a, ctxt, loc, "NULL a");
  RETURN_NULL_IF_FAIL (b, ctxt, loc, "NULL b");
  RETURN_NULL_IF_FAIL_PRINTF4 (
    a->get_type ()->unqualified () == b->get_type ()->unqualified (),
    ctxt, loc,
    "mismatching types for binary op:"
    " a: %s (type: %s) b: %s (type: %s)",
    a->get_debug_string (),
    a->get_type ()->get_debug_string (),
    b->get_debug_string (),
    b->get_type ()->get_debug_string ());
  RETURN_NULL_IF_FAIL_PRINTF4 (
    result_type->is_numeric (), ctxt, loc,
    "gcc_jit_binary_op %s with operands a: %s b: %s "
    "has non-numeric result_type: %s",
    gcc::jit::binary_op_reproducer_strings[op],
    a->get_debug_string (), b->get_debug_string (),
    result_type->get_debug_string ());

  return (gcc_jit_rvalue *)ctxt->new_binary_op (loc, op, result_type, a, b);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_comparison method in
   jit-recording.c.  */

gcc_jit_rvalue *
gcc_jit_context_new_comparison (gcc_jit_context *ctxt,
				gcc_jit_location *loc,
				enum gcc_jit_comparison op,
				gcc_jit_rvalue *a, gcc_jit_rvalue *b)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, loc, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL_PRINTF1 (
    (op >= GCC_JIT_COMPARISON_EQ
     && op <= GCC_JIT_COMPARISON_GE),
    ctxt, loc,
    "unrecognized value for enum gcc_jit_comparison: %i",
    op);
  RETURN_NULL_IF_FAIL (a, ctxt, loc, "NULL a");
  RETURN_NULL_IF_FAIL (b, ctxt, loc, "NULL b");
  RETURN_NULL_IF_FAIL_PRINTF4 (
    a->get_type ()->unqualified () == b->get_type ()->unqualified (),
    ctxt, loc,
    "mismatching types for comparison:"
    " a: %s (type: %s) b: %s (type: %s)",
    a->get_debug_string (),
    a->get_type ()->get_debug_string (),
    b->get_debug_string (),
    b->get_type ()->get_debug_string ());

  return (gcc_jit_rvalue *)ctxt->new_comparison (loc, op, a, b);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_call method in
   jit-recording.c.  */

gcc_jit_rvalue *
gcc_jit_context_new_call (gcc_jit_context *ctxt,
			  gcc_jit_location *loc,
			  gcc_jit_function *func,
			  int numargs , gcc_jit_rvalue **args)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, loc, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (func, ctxt, loc, "NULL function");
  if (numargs)
    RETURN_NULL_IF_FAIL (args, ctxt, loc, "NULL args");

  int min_num_params = func->get_params ().length ();
  bool is_variadic = func->is_variadic ();

  RETURN_NULL_IF_FAIL_PRINTF3 (
    numargs >= min_num_params,
    ctxt, loc,
    "not enough arguments to function \"%s\""
    " (got %i args, expected %i)",
    func->get_name ()->c_str (),
    numargs, min_num_params);

  RETURN_NULL_IF_FAIL_PRINTF3 (
    (numargs == min_num_params || is_variadic),
    ctxt, loc,
    "too many arguments to function \"%s\""
    " (got %i args, expected %i)",
    func->get_name ()->c_str (),
    numargs, min_num_params);

  for (int i = 0; i < min_num_params; i++)
    {
      gcc::jit::recording::param *param = func->get_param (i);
      gcc_jit_rvalue *arg = args[i];

      RETURN_NULL_IF_FAIL_PRINTF4 (
	arg,
	ctxt, loc,
	"NULL argument %i to function \"%s\":"
	" param %s (type: %s)",
	i + 1,
	func->get_name ()->c_str (),
	param->get_debug_string (),
	param->get_type ()->get_debug_string ());

      RETURN_NULL_IF_FAIL_PRINTF6 (
	compatible_types (param->get_type (),
			  arg->get_type ()),
	ctxt, loc,
	"mismatching types for argument %d of function \"%s\":"
	" assignment to param %s (type: %s) from %s (type: %s)",
	i + 1,
	func->get_name ()->c_str (),
	param->get_debug_string (),
	param->get_type ()->get_debug_string (),
	arg->get_debug_string (),
	arg->get_type ()->get_debug_string ());
    }

  return (gcc_jit_rvalue *)ctxt->new_call (loc,
					   func,
					   numargs,
					   (gcc::jit::recording::rvalue **)args);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_call_through_ptr method in
   jit-recording.c.  */

gcc_jit_rvalue *
gcc_jit_context_new_call_through_ptr (gcc_jit_context *ctxt,
				      gcc_jit_location *loc,
				      gcc_jit_rvalue *fn_ptr,
				      int numargs, gcc_jit_rvalue **args)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, loc, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (fn_ptr, ctxt, loc, "NULL fn_ptr");
  if (numargs)
    RETURN_NULL_IF_FAIL (args, ctxt, loc, "NULL args");

  gcc::jit::recording::type *ptr_type = fn_ptr->get_type ()->dereference ();
  RETURN_NULL_IF_FAIL_PRINTF2 (
    ptr_type, ctxt, loc,
    "fn_ptr is not a ptr: %s"
    " type: %s",
    fn_ptr->get_debug_string (),
    fn_ptr->get_type ()->get_debug_string ());

  gcc::jit::recording::function_type *fn_type =
    ptr_type->dyn_cast_function_type();
  RETURN_NULL_IF_FAIL_PRINTF2 (
    fn_type, ctxt, loc,
    "fn_ptr is not a function ptr: %s"
    " type: %s",
    fn_ptr->get_debug_string (),
    fn_ptr->get_type ()->get_debug_string ());

  int min_num_params = fn_type->get_param_types ().length ();
  bool is_variadic = fn_type->is_variadic ();

  RETURN_NULL_IF_FAIL_PRINTF3 (
    numargs >= min_num_params,
    ctxt, loc,
    "not enough arguments to fn_ptr: %s"
    " (got %i args, expected %i)",
    fn_ptr->get_debug_string (),
    numargs, min_num_params);

  RETURN_NULL_IF_FAIL_PRINTF3 (
    (numargs == min_num_params || is_variadic),
    ctxt, loc,
    "too many arguments to fn_ptr: %s"
    " (got %i args, expected %i)",
    fn_ptr->get_debug_string (),
    numargs, min_num_params);

  for (int i = 0; i < min_num_params; i++)
    {
      gcc::jit::recording::type *param_type = fn_type->get_param_types ()[i];
      gcc_jit_rvalue *arg = args[i];

      RETURN_NULL_IF_FAIL_PRINTF3 (
	arg,
	ctxt, loc,
	"NULL argument %i to fn_ptr: %s"
	" (type: %s)",
	i + 1,
	fn_ptr->get_debug_string (),
	param_type->get_debug_string ());

      RETURN_NULL_IF_FAIL_PRINTF6 (
	compatible_types (param_type,
			  arg->get_type ()),
	ctxt, loc,
	"mismatching types for argument %d of fn_ptr: %s:"
	" assignment to param %d (type: %s) from %s (type: %s)",
	i + 1,
	fn_ptr->get_debug_string (),
	i + 1,
	param_type->get_debug_string (),
	arg->get_debug_string (),
	arg->get_type ()->get_debug_string ());
    }

  return (gcc_jit_rvalue *)(
	    ctxt->new_call_through_ptr (loc,
					fn_ptr,
					numargs,
					(gcc::jit::recording::rvalue **)args));
}

/* Helper function for determining if we can cast an rvalue from SRC_TYPE
   to DST_TYPE, for use by gcc_jit_context_new_cast.

   We only permit these kinds of cast:

     int <-> float
     int <-> bool
     P*  <-> Q*   for pointer types P and Q.  */

static bool
is_valid_cast (gcc::jit::recording::type *src_type,
	       gcc_jit_type *dst_type)
{
  bool src_is_int = src_type->is_int ();
  bool dst_is_int = dst_type->is_int ();
  bool src_is_float = src_type->is_float ();
  bool dst_is_float = dst_type->is_float ();
  bool src_is_bool = src_type->is_bool ();
  bool dst_is_bool = dst_type->is_bool ();

  if (src_is_int)
    if (dst_is_int || dst_is_float || dst_is_bool)
      return true;

  if (src_is_float)
    if (dst_is_int || dst_is_float)
      return true;

  if (src_is_bool)
    if (dst_is_int || dst_is_bool)
      return true;

  /* Permit casts between pointer types.  */
  gcc::jit::recording::type *deref_src_type = src_type->is_pointer ();
  gcc::jit::recording::type *deref_dst_type = dst_type->is_pointer ();
  if (deref_src_type && deref_dst_type)
    return true;

  return false;
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_cast method in jit-recording.c.  */

gcc_jit_rvalue *
gcc_jit_context_new_cast (gcc_jit_context *ctxt,
			  gcc_jit_location *loc,
			  gcc_jit_rvalue *rvalue,
			  gcc_jit_type *type)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, loc, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (rvalue, ctxt, loc, "NULL rvalue");
  RETURN_NULL_IF_FAIL (type, ctxt, loc, "NULL type");
  RETURN_NULL_IF_FAIL_PRINTF3 (
    is_valid_cast (rvalue->get_type (), type),
    ctxt, loc,
    "cannot cast %s from type: %s to type: %s",
    rvalue->get_debug_string (),
    rvalue->get_type ()->get_debug_string (),
    type->get_debug_string ());

  return static_cast <gcc_jit_rvalue *> (ctxt->new_cast (loc, rvalue, type));
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_array_access method in
   jit-recording.c.  */

extern gcc_jit_lvalue *
gcc_jit_context_new_array_access (gcc_jit_context *ctxt,
				  gcc_jit_location *loc,
				  gcc_jit_rvalue *ptr,
				  gcc_jit_rvalue *index)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, loc, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (ptr, ctxt, loc, "NULL ptr");
  RETURN_NULL_IF_FAIL (index, ctxt, loc, "NULL index");
  RETURN_NULL_IF_FAIL_PRINTF2 (
    ptr->get_type ()->dereference (),
    ctxt, loc,
    "ptr: %s (type: %s) is not a pointer or array",
    ptr->get_debug_string (),
    ptr->get_type ()->get_debug_string ());
  RETURN_NULL_IF_FAIL_PRINTF2 (
    index->get_type ()->is_numeric (),
    ctxt, loc,
    "index: %s (type: %s) is not of numeric type",
    index->get_debug_string (),
    index->get_type ()->get_debug_string ());

  return (gcc_jit_lvalue *)ctxt->new_array_access (loc, ptr, index);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::memento::get_context method in
   jit-recording.h.  */

gcc_jit_context *
gcc_jit_object_get_context (gcc_jit_object *obj)
{
  RETURN_NULL_IF_FAIL (obj, NULL, NULL, "NULL object");

  return static_cast <gcc_jit_context *> (obj->get_context ());
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::memento::get_debug_string method in
   jit-recording.c.  */

const char *
gcc_jit_object_get_debug_string (gcc_jit_object *obj)
{
  RETURN_NULL_IF_FAIL (obj, NULL, NULL, "NULL object");

  return obj->get_debug_string ();
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::lvalue::access_field method in
   jit-recording.c.  */

gcc_jit_lvalue *
gcc_jit_lvalue_access_field (gcc_jit_lvalue *struct_,
			     gcc_jit_location *loc,
			     gcc_jit_field *field)
{
  RETURN_NULL_IF_FAIL (struct_, NULL, loc, "NULL struct");
  gcc::jit::recording::context *ctxt = struct_->m_ctxt;
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (field, ctxt, loc, "NULL field");
  RETURN_NULL_IF_FAIL_PRINTF1 (field->get_container (), field->m_ctxt, loc,
			       "field %s has not been placed in a struct",
			       field->get_debug_string ());
  gcc::jit::recording::type *underlying_type =
    struct_->get_type ();
  RETURN_NULL_IF_FAIL_PRINTF2 (
    (field->get_container ()->unqualified ()
     == underlying_type->unqualified ()),
    struct_->m_ctxt, loc,
    "%s is not a field of %s",
    field->get_debug_string (),
    underlying_type->get_debug_string ());

  return (gcc_jit_lvalue *)struct_->access_field (loc, field);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::rvalue::access_field method in
   jit-recording.c.  */

gcc_jit_rvalue *
gcc_jit_rvalue_access_field (gcc_jit_rvalue *struct_,
			     gcc_jit_location *loc,
			     gcc_jit_field *field)
{
  RETURN_NULL_IF_FAIL (struct_, NULL, loc, "NULL struct");
  gcc::jit::recording::context *ctxt = struct_->m_ctxt;
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (field, ctxt, loc, "NULL field");
  RETURN_NULL_IF_FAIL_PRINTF1 (field->get_container (), field->m_ctxt, loc,
			       "field %s has not been placed in a struct",
			       field->get_debug_string ());
  gcc::jit::recording::type *underlying_type =
    struct_->get_type ();
  RETURN_NULL_IF_FAIL_PRINTF2 (
    (field->get_container ()->unqualified ()
     == underlying_type->unqualified ()),
    struct_->m_ctxt, loc,
    "%s is not a field of %s",
    field->get_debug_string (),
    underlying_type->get_debug_string ());

  return (gcc_jit_rvalue *)struct_->access_field (loc, field);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::rvalue::deference_field method in
   jit-recording.c.  */

gcc_jit_lvalue *
gcc_jit_rvalue_dereference_field (gcc_jit_rvalue *ptr,
				  gcc_jit_location *loc,
				  gcc_jit_field *field)
{
  RETURN_NULL_IF_FAIL (ptr, NULL, loc, "NULL ptr");
  JIT_LOG_FUNC (ptr->get_context ()->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (field, NULL, loc, "NULL field");
  gcc::jit::recording::type *underlying_type =
    ptr->get_type ()->is_pointer ();
  RETURN_NULL_IF_FAIL_PRINTF1 (field->get_container (), field->m_ctxt, loc,
			       "field %s has not been placed in a struct",
			       field->get_debug_string ());
  RETURN_NULL_IF_FAIL_PRINTF3 (
    underlying_type,
    ptr->m_ctxt, loc,
    "dereference of non-pointer %s (type: %s) when accessing ->%s",
    ptr->get_debug_string (),
    ptr->get_type ()->get_debug_string (),
    field->get_debug_string ());
  RETURN_NULL_IF_FAIL_PRINTF2 (
    (field->get_container ()->unqualified ()
     == underlying_type->unqualified ()),
    ptr->m_ctxt, loc,
    "%s is not a field of %s",
    field->get_debug_string (),
    underlying_type->get_debug_string ());

  return (gcc_jit_lvalue *)ptr->dereference_field (loc, field);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::rvalue::deference method in
   jit-recording.c.  */

gcc_jit_lvalue *
gcc_jit_rvalue_dereference (gcc_jit_rvalue *rvalue,
			    gcc_jit_location *loc)
{
  RETURN_NULL_IF_FAIL (rvalue, NULL, loc, "NULL rvalue");
  JIT_LOG_FUNC (rvalue->get_context ()->get_logger ());
  /* LOC can be NULL.  */

  gcc::jit::recording::type *underlying_type =
    rvalue->get_type ()->is_pointer ();

  RETURN_NULL_IF_FAIL_PRINTF2 (
    underlying_type,
    rvalue->m_ctxt, loc,
    "dereference of non-pointer %s (type: %s)",
    rvalue->get_debug_string (),
    rvalue->get_type ()->get_debug_string ());

  RETURN_NULL_IF_FAIL_PRINTF2 (
    !underlying_type->is_void (),
    rvalue->m_ctxt, loc,
    "dereference of void pointer %s (type: %s)",
    rvalue->get_debug_string (),
    rvalue->get_type ()->get_debug_string ());

  return (gcc_jit_lvalue *)rvalue->dereference (loc);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::lvalue::get_address method in jit-recording.c.  */

gcc_jit_rvalue *
gcc_jit_lvalue_get_address (gcc_jit_lvalue *lvalue,
			    gcc_jit_location *loc)
{
  RETURN_NULL_IF_FAIL (lvalue, NULL, loc, "NULL lvalue");
  JIT_LOG_FUNC (lvalue->get_context ()->get_logger ());
  /* LOC can be NULL.  */

  return (gcc_jit_rvalue *)lvalue->get_address (loc);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::function::new_local method in jit-recording.c.  */

gcc_jit_lvalue *
gcc_jit_function_new_local (gcc_jit_function *func,
			    gcc_jit_location *loc,
			    gcc_jit_type *type,
			    const char *name)
{
  RETURN_NULL_IF_FAIL (func, NULL, loc, "NULL function");
  gcc::jit::recording::context *ctxt = func->m_ctxt;
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (func->get_kind () != GCC_JIT_FUNCTION_IMPORTED,
		       ctxt, loc,
		       "Cannot add locals to an imported function");
  RETURN_NULL_IF_FAIL (type, ctxt, loc, "NULL type");
  RETURN_NULL_IF_FAIL (name, ctxt, loc, "NULL name");
  RETURN_NULL_IF_FAIL_PRINTF2 (
    type->has_known_size (),
    ctxt, loc,
    "unknown size for local \"%s\" (type: %s)",
    name,
    type->get_debug_string ());
  RETURN_NULL_IF_FAIL_PRINTF1 (
    !type->is_void (),
    ctxt, loc,
    "void type for local \"%s\"",
    name);

  return (gcc_jit_lvalue *)func->new_local (loc, type, name);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::block::add_eval method in jit-recording.c.  */

void
gcc_jit_block_add_eval (gcc_jit_block *block,
			gcc_jit_location *loc,
			gcc_jit_rvalue *rvalue)
{
  RETURN_IF_NOT_VALID_BLOCK (block, loc);
  gcc::jit::recording::context *ctxt = block->get_context ();
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_IF_FAIL (rvalue, ctxt, loc, "NULL rvalue");

  gcc::jit::recording::statement *stmt = block->add_eval (loc, rvalue);

  /* "stmt" should be good enough to be usable in error-messages,
     but might still not be compilable; perform some more
     error-checking here.  We do this here so that the error messages
     can contain a stringified version of "stmt", whilst appearing
     as close as possible to the point of failure.  */
  rvalue->verify_valid_within_stmt (__func__, stmt);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::block::add_assignment method in
   jit-recording.c.  */

void
gcc_jit_block_add_assignment (gcc_jit_block *block,
			      gcc_jit_location *loc,
			      gcc_jit_lvalue *lvalue,
			      gcc_jit_rvalue *rvalue)
{
  RETURN_IF_NOT_VALID_BLOCK (block, loc);
  gcc::jit::recording::context *ctxt = block->get_context ();
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_IF_FAIL (lvalue, ctxt, loc, "NULL lvalue");
  RETURN_IF_FAIL (rvalue, ctxt, loc, "NULL rvalue");
  RETURN_IF_FAIL_PRINTF4 (
    compatible_types (lvalue->get_type (),
		      rvalue->get_type ()),
    ctxt, loc,
    "mismatching types:"
    " assignment to %s (type: %s) from %s (type: %s)",
    lvalue->get_debug_string (),
    lvalue->get_type ()->get_debug_string (),
    rvalue->get_debug_string (),
    rvalue->get_type ()->get_debug_string ());

  gcc::jit::recording::statement *stmt = block->add_assignment (loc, lvalue, rvalue);

  /* "stmt" should be good enough to be usable in error-messages,
     but might still not be compilable; perform some more
     error-checking here.  We do this here so that the error messages
     can contain a stringified version of "stmt", whilst appearing
     as close as possible to the point of failure.  */
  lvalue->verify_valid_within_stmt (__func__, stmt);
  rvalue->verify_valid_within_stmt (__func__, stmt);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::block::add_assignment_op method in
   jit-recording.c.  */

void
gcc_jit_block_add_assignment_op (gcc_jit_block *block,
				 gcc_jit_location *loc,
				 gcc_jit_lvalue *lvalue,
				 enum gcc_jit_binary_op op,
				 gcc_jit_rvalue *rvalue)
{
  RETURN_IF_NOT_VALID_BLOCK (block, loc);
  gcc::jit::recording::context *ctxt = block->get_context ();
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_IF_FAIL (lvalue, ctxt, loc, "NULL lvalue");
  RETURN_IF_FAIL_PRINTF1 (
    valid_binary_op_p (op),
    ctxt, loc,
    "unrecognized value for enum gcc_jit_binary_op: %i",
    op);
  RETURN_IF_FAIL (rvalue, ctxt, loc, "NULL rvalue");
  RETURN_IF_FAIL_PRINTF4 (
    compatible_types (lvalue->get_type (),
		      rvalue->get_type ()),
    ctxt, loc,
    "mismatching types:"
    " assignment to %s (type: %s) involving %s (type: %s)",
    lvalue->get_debug_string (),
    lvalue->get_type ()->get_debug_string (),
    rvalue->get_debug_string (),
    rvalue->get_type ()->get_debug_string ());

  gcc::jit::recording::statement *stmt = block->add_assignment_op (loc, lvalue, op, rvalue);

  /* "stmt" should be good enough to be usable in error-messages,
     but might still not be compilable; perform some more
     error-checking here.  We do this here so that the error messages
     can contain a stringified version of "stmt", whilst appearing
     as close as possible to the point of failure.  */
  lvalue->verify_valid_within_stmt (__func__, stmt);
  rvalue->verify_valid_within_stmt (__func__, stmt);
}

/* Internal helper function for determining if rvalue BOOLVAL is of
   boolean type.  For use by gcc_jit_block_end_with_conditional.  */

static bool
is_bool (gcc_jit_rvalue *boolval)
{
  gcc::jit::recording::type *actual_type = boolval->get_type ();
  gcc::jit::recording::type *bool_type =
    boolval->m_ctxt->get_type (GCC_JIT_TYPE_BOOL);
  return actual_type == bool_type;
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::block::end_with_conditional method in
   jit-recording.c.  */

void
gcc_jit_block_end_with_conditional (gcc_jit_block *block,
				    gcc_jit_location *loc,
				    gcc_jit_rvalue *boolval,
				    gcc_jit_block *on_true,
				    gcc_jit_block *on_false)
{
  RETURN_IF_NOT_VALID_BLOCK (block, loc);
  gcc::jit::recording::context *ctxt = block->get_context ();
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_IF_FAIL (boolval, ctxt, loc, "NULL boolval");
  RETURN_IF_FAIL_PRINTF2 (
   is_bool (boolval), ctxt, loc,
   "%s (type: %s) is not of boolean type ",
   boolval->get_debug_string (),
   boolval->get_type ()->get_debug_string ());
  RETURN_IF_FAIL (on_true, ctxt, loc, "NULL on_true");
  RETURN_IF_FAIL (on_true, ctxt, loc, "NULL on_false");
  RETURN_IF_FAIL_PRINTF4 (
    block->get_function () == on_true->get_function (),
    ctxt, loc,
    "\"on_true\" block is not in same function:"
    " source block %s is in function %s"
    " whereas target block %s is in function %s",
    block->get_debug_string (),
    block->get_function ()->get_debug_string (),
    on_true->get_debug_string (),
    on_true->get_function ()->get_debug_string ());
  RETURN_IF_FAIL_PRINTF4 (
    block->get_function () == on_false->get_function (),
    ctxt, loc,
    "\"on_false\" block is not in same function:"
    " source block %s is in function %s"
    " whereas target block %s is in function %s",
    block->get_debug_string (),
    block->get_function ()->get_debug_string (),
    on_false->get_debug_string (),
    on_false->get_function ()->get_debug_string ());

  gcc::jit::recording::statement *stmt = block->end_with_conditional (loc, boolval, on_true, on_false);

  /* "stmt" should be good enough to be usable in error-messages,
     but might still not be compilable; perform some more
     error-checking here.  We do this here so that the error messages
     can contain a stringified version of "stmt", whilst appearing
     as close as possible to the point of failure.  */
  boolval->verify_valid_within_stmt (__func__, stmt);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::block::add_comment method in
   jit-recording.c.  */

void
gcc_jit_block_add_comment (gcc_jit_block *block,
			   gcc_jit_location *loc,
			   const char *text)
{
  RETURN_IF_NOT_VALID_BLOCK (block, loc);
  gcc::jit::recording::context *ctxt = block->get_context ();
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_IF_FAIL (text, ctxt, loc, "NULL text");

  block->add_comment (loc, text);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::block::end_with_jump method in
   jit-recording.c.  */

void
gcc_jit_block_end_with_jump (gcc_jit_block *block,
			     gcc_jit_location *loc,
			     gcc_jit_block *target)
{
  RETURN_IF_NOT_VALID_BLOCK (block, loc);
  gcc::jit::recording::context *ctxt = block->get_context ();
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_IF_FAIL (target, ctxt, loc, "NULL target");
  RETURN_IF_FAIL_PRINTF4 (
    block->get_function () == target->get_function (),
    ctxt, loc,
    "target block is not in same function:"
    " source block %s is in function %s"
    " whereas target block %s is in function %s",
    block->get_debug_string (),
    block->get_function ()->get_debug_string (),
    target->get_debug_string (),
    target->get_function ()->get_debug_string ());

  block->end_with_jump (loc, target);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::block::end_with_return method in
   jit-recording.c.  */

void
gcc_jit_block_end_with_return (gcc_jit_block *block,
			       gcc_jit_location *loc,
			       gcc_jit_rvalue *rvalue)
{
  RETURN_IF_NOT_VALID_BLOCK (block, loc);
  gcc::jit::recording::context *ctxt = block->get_context ();
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  gcc::jit::recording::function *func = block->get_function ();
  RETURN_IF_FAIL (rvalue, ctxt, loc, "NULL rvalue");
  RETURN_IF_FAIL_PRINTF4 (
    compatible_types (
      func->get_return_type (),
      rvalue->get_type ()),
    ctxt, loc,
    "mismatching types:"
    " return of %s (type: %s) in function %s (return type: %s)",
    rvalue->get_debug_string (),
    rvalue->get_type ()->get_debug_string (),
    func->get_debug_string (),
    func->get_return_type ()->get_debug_string ());

  gcc::jit::recording::statement *stmt = block->end_with_return (loc, rvalue);

  /* "stmt" should be good enough to be usable in error-messages,
     but might still not be compilable; perform some more
     error-checking here.  We do this here so that the error messages
     can contain a stringified version of "stmt", whilst appearing
     as close as possible to the point of failure.  */
  rvalue->verify_valid_within_stmt (__func__, stmt);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::block::end_with_return method in
   jit-recording.c.  */

void
gcc_jit_block_end_with_void_return (gcc_jit_block *block,
				    gcc_jit_location *loc)
{
  RETURN_IF_NOT_VALID_BLOCK (block, loc);
  gcc::jit::recording::context *ctxt = block->get_context ();
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  gcc::jit::recording::function *func = block->get_function ();
  RETURN_IF_FAIL_PRINTF2 (
    func->get_return_type () == ctxt->get_type (GCC_JIT_TYPE_VOID),
    ctxt, loc,
    "mismatching types:"
    " void return in function %s (return type: %s)",
    func->get_debug_string (),
    func->get_return_type ()->get_debug_string ());

  block->end_with_return (loc, NULL);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_case method in
   jit-recording.c.  */

gcc_jit_case *
gcc_jit_context_new_case (gcc_jit_context *ctxt,
			  gcc_jit_rvalue *min_value,
			  gcc_jit_rvalue *max_value,
			  gcc_jit_block *block)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_NULL_IF_FAIL (min_value, ctxt, NULL, "NULL min_value");
  RETURN_NULL_IF_FAIL (max_value, ctxt, NULL, "NULL max_value");
  RETURN_NULL_IF_FAIL (block, ctxt, NULL, "NULL block");

  RETURN_NULL_IF_FAIL_PRINTF1 (min_value->is_constant (), ctxt, NULL,
			       "min_value is not a constant: %s",
			       min_value->get_debug_string ());
  RETURN_NULL_IF_FAIL_PRINTF1 (max_value->is_constant (), ctxt, NULL,
			       "max_value is not a constant: %s",
			       max_value->get_debug_string ());
  RETURN_NULL_IF_FAIL_PRINTF2 (
    min_value->get_type ()->is_int (),
    ctxt, NULL,
    "min_value: %s (type: %s) is not of integer type",
    min_value->get_debug_string (),
    min_value->get_type ()->get_debug_string ());
  RETURN_NULL_IF_FAIL_PRINTF2 (
    max_value->get_type ()->is_int (),
    ctxt, NULL,
    "max_value: %s (type: %s) is not of integer type",
    max_value->get_debug_string (),
    max_value->get_type ()->get_debug_string ());

  wide_int wi_min, wi_max;
  if (!min_value->get_wide_int (&wi_min))
    gcc_unreachable ();
  if (!max_value->get_wide_int (&wi_max))
    gcc_unreachable ();
  RETURN_NULL_IF_FAIL_PRINTF2 (
    wi::les_p (wi_min, wi_max),
    ctxt, NULL,
    "min_value: %s > max_value: %s",
    min_value->get_debug_string (),
    max_value->get_debug_string ());
  return (gcc_jit_case *)ctxt->new_case (min_value,
					 max_value,
					 block);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, this calls the trivial
   gcc::jit::recording::memento::as_object method (a case is a
   memento), in jit-recording.h.  */

gcc_jit_object *
gcc_jit_case_as_object (gcc_jit_case *case_)
{
  RETURN_NULL_IF_FAIL (case_, NULL, NULL, "NULL case");

  return static_cast <gcc_jit_object *> (case_->as_object ());
}

/* Helper function for gcc_jit_block_end_with_switch and
   valid_case_for_switch.  */

static bool
valid_dest_for_switch (gcc::jit::recording::context *ctxt,
		       gcc_jit_location *loc,
		       const char *api_funcname,
		       gcc::jit::recording::block *switch_block,
		       gcc::jit::recording::block *dest_block,
		       const char *dest_block_desc)
{
  if (!dest_block)
    {
      jit_error (ctxt, loc, "%s: NULL %s", api_funcname, dest_block_desc);
      return false;
    }
  gcc::jit::recording::function *switch_fn = switch_block->get_function ();
  gcc::jit::recording::function *dest_fn = dest_block->get_function ();
  if (switch_fn != dest_fn)
    {
      jit_error (ctxt, loc,
		 "%s: %s is not in same function:"
		 " switch block %s is in function %s"
		 " whereas %s %s is in function %s",
		 api_funcname,
		 dest_block_desc,
		 switch_block->get_debug_string (),
		 switch_fn->get_debug_string (),
		 dest_block_desc,
		 dest_block->get_debug_string (),
		 dest_fn->get_debug_string ());
      return false;
    }
  return true;
}

/* Helper function for gcc_jit_block_end_with_switch.  */

static bool
valid_case_for_switch (gcc::jit::recording::context *ctxt,
		       gcc_jit_location *loc,
		       const char *api_funcname,
		       gcc_jit_block *switch_block,
		       gcc_jit_rvalue *expr,
		       gcc_jit_case *case_,
		       const char *case_desc,
		       int case_idx)
{
  if (!case_)
    {
      jit_error (ctxt, loc,
		 "%s:"
		 " NULL case %i",
		 api_funcname,
		 case_idx);
      return false;
    }
  if (!valid_dest_for_switch (ctxt, loc,
			      api_funcname,
			      switch_block,
			      case_->get_dest_block (),
			      case_desc))
    return false;
  gcc::jit::recording::type *expr_type = expr->get_type ();
  if (expr_type != case_->get_min_value ()->get_type ())
    {
      jit_error (ctxt, loc,
		 "%s:"
		 " mismatching types between case and expression:"
		 " cases[%i]->min_value: %s (type: %s)"
		 " expr: %s (type: %s)",
		 api_funcname,
		 case_idx,
		 case_->get_min_value ()->get_debug_string (),
		 case_->get_min_value ()->get_type ()->get_debug_string (),
		 expr->get_debug_string (),
		 expr_type->get_debug_string ());
      return false;
    }
  if (expr_type != case_->get_max_value ()->get_type ())
    {
      jit_error (ctxt, loc,
		 "%s:"
		 " mismatching types between case and expression:"
		 " cases[%i]->max_value: %s (type: %s)"
		 " expr: %s (type: %s)",
		 api_funcname,
		 case_idx,
		 case_->get_max_value ()->get_debug_string (),
		 case_->get_max_value ()->get_type ()->get_debug_string (),
		 expr->get_debug_string (),
		 expr_type->get_debug_string ());
      return false;
    }
  return true;
}

/* A class for holding the data we need to perform error-checking
   on a libgccjit API call.  */

class api_call_validator
{
 public:
  api_call_validator (gcc::jit::recording::context *ctxt,
		      gcc_jit_location *loc,
		      const char *funcname)
  : m_ctxt (ctxt),
    m_loc (loc),
    m_funcname (funcname)
  {}

 protected:
  gcc::jit::recording::context *m_ctxt;
  gcc_jit_location *m_loc;
  const char *m_funcname;
};

/* A class for verifying that the ranges of cases within
   gcc_jit_block_end_with_switch don't overlap.  */

class case_range_validator : public api_call_validator
{
 public:
  case_range_validator (gcc::jit::recording::context *ctxt,
			gcc_jit_location *loc,
			const char *funcname);

  bool
  validate (gcc_jit_case *case_, int idx);

 private:
  static int
  case_compare (gcc::jit::recording::rvalue *k1,
		gcc::jit::recording::rvalue *k2);

  static wide_int
  get_wide_int (gcc::jit::recording::rvalue *k);

 private:
  typed_splay_tree <gcc::jit::recording::rvalue *, gcc_jit_case *> m_cases;
};

/* case_range_validator's ctor.  */

case_range_validator::case_range_validator (gcc::jit::recording::context *ctxt,
					    gcc_jit_location *loc,
					    const char *funcname)
: api_call_validator (ctxt, loc, funcname),
  m_cases (case_compare, NULL, NULL)
{
}

/* Ensure that the range of CASE_ does not overlap with any of the
   ranges of cases we've already seen.
   Return true if everything is OK.
   Return false and emit an error if there is an overlap.
   Compare with c-family/c-common.c:c_add_case_label.  */

bool
case_range_validator::validate (gcc_jit_case *case_,
				int case_idx)
{
  /* Look up the LOW_VALUE in the table of case labels we already
     have.  */
  gcc_jit_case *other = m_cases.lookup (case_->get_min_value ());

  /* If there was not an exact match, check for overlapping ranges.  */
  if (!other)
    {
      gcc_jit_case *pred;
      gcc_jit_case *succ;

      /* Even though there wasn't an exact match, there might be an
	 overlap between this case range and another case range.
	 Since we've (inductively) not allowed any overlapping case
	 ranges, we simply need to find the greatest low case label
	 that is smaller that CASE_MIN_VALUE, and the smallest low case
	 label that is greater than CASE_MAX_VALUE.  If there is an overlap
	 it will occur in one of these two ranges.  */
      pred = m_cases.predecessor (case_->get_min_value ());
      succ = m_cases.successor (case_->get_max_value ());

      /* Check to see if the PRED overlaps.  It is smaller than
	 the LOW_VALUE, so we only need to check its max value.  */
      if (pred)
	{
	  wide_int wi_case_min = get_wide_int (case_->get_min_value ());
	  wide_int wi_pred_max = get_wide_int (pred->get_max_value ());
	  if (wi::ges_p (wi_pred_max, wi_case_min))
	    other = pred;
	}

      if (!other && succ)
	{
	  /* Check to see if the SUCC overlaps.  The low end of that
	     range is bigger than the low end of the current range.  */
	  wide_int wi_case_max = get_wide_int (case_->get_max_value ());
	  wide_int wi_succ_min = get_wide_int (succ->get_min_value ());
	  if (wi::les_p (wi_succ_min, wi_case_max))
	    other = succ;
	}
    }

  /* If there was an overlap, issue an error.  */
  if (other)
    {
      jit_error (m_ctxt, m_loc,
		 "%s: duplicate (or overlapping) cases values:"
		 " case %i: %s overlaps %s",
		 m_funcname,
		 case_idx,
		 case_->get_debug_string (),
		 other->get_debug_string ());
      return false;
    }

  /* Register this case label in the splay tree.  */
  m_cases.insert (case_->get_min_value (),
		  case_);
  return true;
}

/* Compare with c-family/c-common.c:case_compare, which acts on tree
   nodes, rather than rvalue *.

   Comparator for case label values.  K1 and K2 must be constant integer
   values (anything else should have been rejected by
   gcc_jit_context_new_case.

   Returns -1 if K1 is ordered before K2, -1 if K1 is ordered after
   K2, and 0 if K1 and K2 are equal.  */

int
case_range_validator::case_compare (gcc::jit::recording::rvalue * k1,
				    gcc::jit::recording::rvalue * k2)
{
  wide_int wi1 = get_wide_int (k1);
  wide_int wi2 = get_wide_int (k2);
  return wi::cmps(wi1, wi2);
}

/* Given a const int rvalue K, get the underlying value as a wide_int.  */

wide_int
case_range_validator::get_wide_int (gcc::jit::recording::rvalue *k)
{
  wide_int wi;
  bool got_wi = k->get_wide_int (&wi);
  gcc_assert (got_wi);
  return wi;
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::block::end_with_switch method in
   jit-recording.c.  */

void
gcc_jit_block_end_with_switch (gcc_jit_block *block,
			       gcc_jit_location *loc,
			       gcc_jit_rvalue *expr,
			       gcc_jit_block *default_block,
			       int num_cases,
			       gcc_jit_case **cases)
{
  RETURN_IF_NOT_VALID_BLOCK (block, loc);
  gcc::jit::recording::context *ctxt = block->get_context ();
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_IF_FAIL (expr, ctxt, loc,
		  "NULL expr");
  gcc::jit::recording::type *expr_type = expr->get_type ();
  RETURN_IF_FAIL_PRINTF2 (
    expr_type->is_int (),
    ctxt, loc,
    "expr: %s (type: %s) is not of integer type",
    expr->get_debug_string (),
    expr_type->get_debug_string ());
  if (!valid_dest_for_switch (ctxt, loc,
			      __func__,
			      block,
			      default_block,
			      "default_block"))
    return;
  RETURN_IF_FAIL (num_cases >= 0, ctxt, loc, "num_cases < 0");
  case_range_validator crv (ctxt, loc, __func__);
  for (int i = 0; i < num_cases; i++)
    {
      char case_desc[32];
      snprintf (case_desc, sizeof (case_desc),
		"cases[%i]", i);
      if (!valid_case_for_switch (ctxt, loc,
				  __func__,
				  block,
				  expr,
				  cases[i],
				  case_desc,
				  i))
	return;
      if (!crv.validate (cases[i], i))
	return;
    }

  block->end_with_switch (loc, expr, default_block,
			  num_cases,
			  (gcc::jit::recording::case_ **)cases);
}

/**********************************************************************
 Option-management
 **********************************************************************/

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::set_str_option method in
   jit-recording.c.  */

void
gcc_jit_context_set_str_option (gcc_jit_context *ctxt,
				enum gcc_jit_str_option opt,
				const char *value)
{
  RETURN_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* opt is checked by the inner function.
     value can be NULL.  */

  ctxt->set_str_option (opt, value);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::set_int_option method in
   jit-recording.c.  */

void
gcc_jit_context_set_int_option (gcc_jit_context *ctxt,
				enum gcc_jit_int_option opt,
				int value)
{
  RETURN_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* opt is checked by the inner function.  */

  ctxt->set_int_option (opt, value);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::set_bool_option method in
   jit-recording.c.  */

void
gcc_jit_context_set_bool_option (gcc_jit_context *ctxt,
				 enum gcc_jit_bool_option opt,
				 int value)
{
  RETURN_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* opt is checked by the inner function.  */

  ctxt->set_bool_option (opt, value);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::set_inner_bool_option method in
   jit-recording.c.  */

void
gcc_jit_context_set_bool_allow_unreachable_blocks (gcc_jit_context *ctxt,
						   int bool_value)
{
  RETURN_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  ctxt->set_inner_bool_option (
    gcc::jit::INNER_BOOL_OPTION_ALLOW_UNREACHABLE_BLOCKS,
    bool_value);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::set_inner_bool_option method in
   jit-recording.c.  */

extern void
gcc_jit_context_set_bool_use_external_driver (gcc_jit_context *ctxt,
					      int bool_value)
{
  RETURN_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  ctxt->set_inner_bool_option (
    gcc::jit::INNER_BOOL_OPTION_USE_EXTERNAL_DRIVER,
    bool_value);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::add_command_line_option method in
   jit-recording.c.  */

void
gcc_jit_context_add_command_line_option (gcc_jit_context *ctxt,
					 const char *optname)
{
  RETURN_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_IF_FAIL (optname, ctxt, NULL, "NULL optname");
  if (ctxt->get_logger ())
    ctxt->get_logger ()->log ("optname: %s", optname);

  ctxt->add_command_line_option (optname);
}

/* Public entrypoint.  See description in libgccjit.h.

   The real work is done by the
   gcc::jit::recording::context::add_driver_option method in
   jit-recording.c.  */

void
gcc_jit_context_add_driver_option (gcc_jit_context *ctxt,
				   const char *optname)
{
  RETURN_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_IF_FAIL (optname, ctxt, NULL, "NULL optname");
  if (ctxt->get_logger ())
    ctxt->get_logger ()->log ("optname: %s", optname);

  ctxt->add_driver_option (optname);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::enable_dump method in
   jit-recording.c.  */

void
gcc_jit_context_enable_dump (gcc_jit_context *ctxt,
			     const char *dumpname,
			     char **out_ptr)
{
  RETURN_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_IF_FAIL (dumpname, ctxt, NULL, "NULL dumpname");
  RETURN_IF_FAIL (out_ptr, ctxt, NULL, "NULL out_ptr");

  ctxt->enable_dump (dumpname, out_ptr);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::compile method in
   jit-recording.c.  */

gcc_jit_result *
gcc_jit_context_compile (gcc_jit_context *ctxt)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");

  JIT_LOG_FUNC (ctxt->get_logger ());

  ctxt->log ("in-memory compile of ctxt: %p", (void *)ctxt);

  gcc_jit_result *result = (gcc_jit_result *)ctxt->compile ();

  ctxt->log ("%s: returning (gcc_jit_result *)%p",
	     __func__, (void *)result);

  return result;
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::compile_to_file method in
   jit-recording.c.  */

void
gcc_jit_context_compile_to_file (gcc_jit_context *ctxt,
				 enum gcc_jit_output_kind output_kind,
				 const char *output_path)
{
  RETURN_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_IF_FAIL_PRINTF1 (
    ((output_kind >= GCC_JIT_OUTPUT_KIND_ASSEMBLER)
     && (output_kind <= GCC_JIT_OUTPUT_KIND_EXECUTABLE)),
    ctxt, NULL,
    "unrecognized output_kind: %i",
    output_kind);
  RETURN_IF_FAIL (output_path, ctxt, NULL, "NULL output_path");

  ctxt->log ("compile_to_file of ctxt: %p", (void *)ctxt);
  ctxt->log ("output_kind: %i", output_kind);
  ctxt->log ("output_path: %s", output_path);

  ctxt->compile_to_file (output_kind, output_path);
}


/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::dump_to_file method in
   jit-recording.c.  */

void
gcc_jit_context_dump_to_file (gcc_jit_context *ctxt,
			      const char *path,
			      int update_locations)
{
  RETURN_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_IF_FAIL (path, ctxt, NULL, "NULL path");
  ctxt->dump_to_file (path, update_locations);
}

/* Public entrypoint.  See description in libgccjit.h.  */

void
gcc_jit_context_set_logfile (gcc_jit_context *ctxt,
			     FILE *logfile,
			     int flags,
			     int verbosity)
{
  RETURN_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_IF_FAIL ((flags == 0), ctxt, NULL, "flags must be 0 for now");
  RETURN_IF_FAIL ((verbosity == 0), ctxt, NULL, "verbosity must be 0 for now");

  gcc::jit::logger *logger;
  if (logfile)
    logger = new gcc::jit::logger (logfile, flags, verbosity);
  else
    logger = NULL;
  ctxt->set_logger (logger);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::dump_reproducer_to_file method in
   jit-recording.c.  */

void
gcc_jit_context_dump_reproducer_to_file (gcc_jit_context *ctxt,
					 const char *path)
{
  RETURN_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());
  RETURN_IF_FAIL (path, ctxt, NULL, "NULL path");
  ctxt->dump_reproducer_to_file (path);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::get_first_error method in
   jit-recording.c.  */

const char *
gcc_jit_context_get_first_error (gcc_jit_context *ctxt)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");
  JIT_LOG_FUNC (ctxt->get_logger ());

  return ctxt->get_first_error ();
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::get_last_error method in
   jit-recording.c.  */

const char *
gcc_jit_context_get_last_error (gcc_jit_context *ctxt)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL context");

  return ctxt->get_last_error ();
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::result::get_code method in jit-result.c.  */

void *
gcc_jit_result_get_code (gcc_jit_result *result,
			 const char *fnname)
{
  RETURN_NULL_IF_FAIL (result, NULL, NULL, "NULL result");
  JIT_LOG_FUNC (result->get_logger ());
  RETURN_NULL_IF_FAIL (fnname, NULL, NULL, "NULL fnname");

  result->log ("locating fnname: %s", fnname);
  void *code = result->get_code (fnname);
  result->log ("%s: returning (void *)%p", __func__, code);

  return code;
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::result::get_global method in jit-result.c.  */

void *
gcc_jit_result_get_global (gcc_jit_result *result,
			   const char *name)
{
  RETURN_NULL_IF_FAIL (result, NULL, NULL, "NULL result");
  JIT_LOG_FUNC (result->get_logger ());
  RETURN_NULL_IF_FAIL (name, NULL, NULL, "NULL name");

  void *global = result->get_global (name);
  result->log ("%s: returning (void *)%p", __func__, global);

  return global;
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, this is essentially a wrapper around the
   destructor for gcc::jit::result in jit-result.c.  */

void
gcc_jit_result_release (gcc_jit_result *result)
{
  RETURN_IF_FAIL (result, NULL, NULL, "NULL result");
  JIT_LOG_FUNC (result->get_logger ());
  result->log ("deleting result: %p", (void *)result);
  delete result;
}

/**********************************************************************
 Timing support.
 **********************************************************************/

/* Create a gcc_jit_timer instance, and start timing.  */

gcc_jit_timer *
gcc_jit_timer_new (void)
{
  gcc_jit_timer *timer = new gcc_jit_timer ();
  timer->start (TV_TOTAL);
  timer->push (TV_JIT_CLIENT_CODE);
  return timer;
}

/* Release a gcc_jit_timer instance.  */

void
gcc_jit_timer_release (gcc_jit_timer *timer)
{
  RETURN_IF_FAIL (timer, NULL, NULL, "NULL timer");

  delete timer;
}

/* Associate a gcc_jit_timer instance with a context.  */

void
gcc_jit_context_set_timer (gcc_jit_context *ctxt,
			   gcc_jit_timer *timer)
{
  RETURN_IF_FAIL (ctxt, NULL, NULL, "NULL ctxt");
  RETURN_IF_FAIL (timer, ctxt, NULL, "NULL timer");

  ctxt->set_timer (timer);
}

/* Get the timer associated with a context (if any).  */

gcc_jit_timer *
gcc_jit_context_get_timer (gcc_jit_context *ctxt)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, NULL, "NULL ctxt");

  return (gcc_jit_timer *)ctxt->get_timer ();
}

/* Push the given item onto the timing stack.  */

void
gcc_jit_timer_push (gcc_jit_timer *timer,
		    const char *item_name)
{
  RETURN_IF_FAIL (timer, NULL, NULL, "NULL timer");
  RETURN_IF_FAIL (item_name, NULL, NULL, "NULL item_name");
  timer->push_client_item (item_name);
}

/* Pop the top item from the timing stack.  */

void
gcc_jit_timer_pop (gcc_jit_timer *timer,
		   const char *item_name)
{
  RETURN_IF_FAIL (timer, NULL, NULL, "NULL timer");

  if (item_name)
    {
      const char *top_item_name = timer->get_topmost_item_name ();

      RETURN_IF_FAIL_PRINTF1
	(top_item_name, NULL, NULL,
	 "pop of empty timing stack (attempting to pop: \"%s\")",
	 item_name);

      RETURN_IF_FAIL_PRINTF2
	(strcmp (item_name, top_item_name) == 0, NULL, NULL,
	 "mismatching item_name:"
	 " top of timing stack: \"%s\","
	 " attempting to pop: \"%s\"",
	 top_item_name,
	 item_name);
    }

  timer->pop_client_item ();
}

/* Print timing information to the given stream about activity since
   the timer was started.  */

void
gcc_jit_timer_print (gcc_jit_timer *timer,
		     FILE *f_out)
{
  RETURN_IF_FAIL (timer, NULL, NULL, "NULL timer");
  RETURN_IF_FAIL (f_out, NULL, NULL, "NULL f_out");

  timer->pop (TV_JIT_CLIENT_CODE);
  timer->stop (TV_TOTAL);
  timer->print (f_out);
  timer->start (TV_TOTAL);
  timer->push (TV_JIT_CLIENT_CODE);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is effectively done by the
   gcc::jit::base_call::set_require_tail_call setter in jit-recording.h.  */

void
gcc_jit_rvalue_set_bool_require_tail_call (gcc_jit_rvalue *rvalue,
					   int require_tail_call)
{
  RETURN_IF_FAIL (rvalue, NULL, NULL, "NULL call");
  JIT_LOG_FUNC (rvalue->get_context ()->get_logger ());

  /* Verify that it's a call.  */
  gcc::jit::recording::base_call *call = rvalue->dyn_cast_base_call ();
  RETURN_IF_FAIL_PRINTF1 (call, NULL, NULL, "not a call: %s",
			  rvalue->get_debug_string ());

  call->set_require_tail_call (require_tail_call);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::type::get_aligned method, in
   jit-recording.c.  */

gcc_jit_type *
gcc_jit_type_get_aligned (gcc_jit_type *type,
			  size_t alignment_in_bytes)
{
  RETURN_NULL_IF_FAIL (type, NULL, NULL, "NULL type");

  gcc::jit::recording::context *ctxt = type->m_ctxt;

  JIT_LOG_FUNC (ctxt->get_logger ());

  RETURN_NULL_IF_FAIL_PRINTF1
    (pow2_or_zerop (alignment_in_bytes), ctxt, NULL,
     "alignment not a power of two: %zi",
     alignment_in_bytes);
  RETURN_NULL_IF_FAIL (!type->is_void (), ctxt, NULL, "void type");

  return (gcc_jit_type *)type->get_aligned (alignment_in_bytes);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::type::get_vector method, in
   jit-recording.c.  */

gcc_jit_type *
gcc_jit_type_get_vector (gcc_jit_type *type, size_t num_units)
{
  RETURN_NULL_IF_FAIL (type, NULL, NULL, "NULL type");

  gcc::jit::recording::context *ctxt = type->m_ctxt;

  JIT_LOG_FUNC (ctxt->get_logger ());

  RETURN_NULL_IF_FAIL_PRINTF1
    (type->is_int () || type->is_float (), ctxt, NULL,
     "type is not integral or floating point: %s",
     type->get_debug_string ());

  RETURN_NULL_IF_FAIL_PRINTF1
    (pow2_or_zerop (num_units), ctxt, NULL,
     "num_units not a power of two: %zi",
     num_units);

  return (gcc_jit_type *)type->get_vector (num_units);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::function::get_address method, in
   jit-recording.c.  */

gcc_jit_rvalue *
gcc_jit_function_get_address (gcc_jit_function *fn,
			      gcc_jit_location *loc)
{
  RETURN_NULL_IF_FAIL (fn, NULL, NULL, "NULL function");

  gcc::jit::recording::context *ctxt = fn->m_ctxt;

  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */

  return (gcc_jit_rvalue *)fn->get_address (loc);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::new_rvalue_from_vector method, in
   jit-recording.c.  */

extern gcc_jit_rvalue *
gcc_jit_context_new_rvalue_from_vector (gcc_jit_context *ctxt,
					gcc_jit_location *loc,
					gcc_jit_type *vec_type,
					size_t num_elements,
					gcc_jit_rvalue **elements)
{
  RETURN_NULL_IF_FAIL (ctxt, NULL, loc, "NULL ctxt");
  JIT_LOG_FUNC (ctxt->get_logger ());

  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (vec_type, ctxt, loc, "NULL vec_type");

  /* "vec_type" must be a vector type.  */
  gcc::jit::recording::vector_type *as_vec_type
    = vec_type->dyn_cast_vector_type ();
  RETURN_NULL_IF_FAIL_PRINTF1 (as_vec_type, ctxt, loc,
			       "%s is not a vector type",
			       vec_type->get_debug_string ());

  /* "num_elements" must match.  */
  RETURN_NULL_IF_FAIL_PRINTF1 (
    num_elements == as_vec_type->get_num_units (), ctxt, loc,
    "num_elements != %zi", as_vec_type->get_num_units ());

  /* "elements must be non-NULL.  */
  RETURN_NULL_IF_FAIL (elements, ctxt, loc, "NULL elements");

  /* Each of "elements" must be non-NULL and of the correct type.  */
  gcc::jit::recording::type *element_type
    = as_vec_type->get_element_type ();
  for (size_t i = 0; i < num_elements; i++)
    {
      RETURN_NULL_IF_FAIL_PRINTF1 (
	elements[i], ctxt, loc, "NULL elements[%zi]", i);
      RETURN_NULL_IF_FAIL_PRINTF4 (
	compatible_types (element_type,
			  elements[i]->get_type ()),
	ctxt, loc,
	"mismatching type for element[%zi] (expected type: %s): %s (type: %s)",
	i,
	element_type->get_debug_string (),
	elements[i]->get_debug_string (),
	elements[i]->get_type ()->get_debug_string ());
    }

  return (gcc_jit_rvalue *)ctxt->new_rvalue_from_vector
    (loc,
     as_vec_type,
     (gcc::jit::recording::rvalue **)elements);
}

/* A mutex around the cached state in parse_basever.
   Ideally this would be within parse_basever, but the mutex is only needed
   by libgccjit.  */

static pthread_mutex_t version_mutex = PTHREAD_MUTEX_INITIALIZER;

struct version_info
{
  /* Default constructor.  Populate via parse_basever,
     guarded by version_mutex.  */
  version_info ()
  {
    pthread_mutex_lock (&version_mutex);
    parse_basever (&major, &minor, &patchlevel);
    pthread_mutex_unlock (&version_mutex);
  }

  int major;
  int minor;
  int patchlevel;
};


extern int
gcc_jit_version_major (void)
{
  version_info vi;
  return vi.major;
}

extern int
gcc_jit_version_minor (void)
{
  version_info vi;
  return vi.minor;
}

extern int
gcc_jit_version_patchlevel (void)
{
  version_info vi;
  return vi.patchlevel;
}

/**********************************************************************
 Asm support.
 **********************************************************************/

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::block::add_extended_asm, in
   jit-recording.c.  */

gcc_jit_extended_asm *
gcc_jit_block_add_extended_asm (gcc_jit_block *block,
				gcc_jit_location *loc,
				const char *asm_template)
{
  RETURN_NULL_IF_NOT_VALID_BLOCK (block, loc);
  gcc::jit::recording::context *ctxt = block->get_context ();
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (asm_template, ctxt, loc, "NULL asm_template");

  return (gcc_jit_extended_asm *)block->add_extended_asm (loc, asm_template);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::block::end_with_extended_asm_goto, in
   jit-recording.c.  */

gcc_jit_extended_asm *
gcc_jit_block_end_with_extended_asm_goto (gcc_jit_block *block,
					  gcc_jit_location *loc,
					  const char *asm_template,
					  int num_goto_blocks,
					  gcc_jit_block **goto_blocks,
					  gcc_jit_block *fallthrough_block)
{
  RETURN_NULL_IF_NOT_VALID_BLOCK (block, loc);
  gcc::jit::recording::context *ctxt = block->get_context ();
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_NULL_IF_FAIL (asm_template, ctxt, loc, "NULL asm_template");
  RETURN_NULL_IF_FAIL (num_goto_blocks >= 0, ctxt, loc, "num_goto_blocks < 0");
  for (int i = 0; i < num_goto_blocks; i++)
    RETURN_NULL_IF_FAIL_PRINTF1 (goto_blocks[i],
				 ctxt, loc,
				 "NULL goto_blocks[%i]", i);
  /* fallthrough_block can be NULL.  */
  return (gcc_jit_extended_asm *)block->end_with_extended_asm_goto
    (loc, asm_template,
     num_goto_blocks, (gcc::jit::recording::block **)goto_blocks,
     fallthrough_block);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, this calls the trivial
   gcc::jit::recording::memento::as_object method (an extended_asm is a
   memento), in jit-recording.h.  */

gcc_jit_object *
gcc_jit_extended_asm_as_object (gcc_jit_extended_asm *ext_asm)
{
  RETURN_NULL_IF_FAIL (ext_asm, NULL, NULL, "NULL ext_asm");

  return static_cast <gcc_jit_object *> (ext_asm->as_object ());
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::extended_asm::set_volatile_flag, in
   jit-recording.c.  */

void
gcc_jit_extended_asm_set_volatile_flag (gcc_jit_extended_asm *ext_asm,
					int flag)
{
  RETURN_IF_FAIL (ext_asm, NULL, NULL, "NULL ext_asm");
  ext_asm->set_volatile_flag (flag);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::extended_asm::set_inline_flag, in
   jit-recording.c.  */

void
gcc_jit_extended_asm_set_inline_flag (gcc_jit_extended_asm *ext_asm,
				      int flag)
{
  RETURN_IF_FAIL (ext_asm, NULL, NULL, "NULL ext_asm");
  ext_asm->set_inline_flag (flag);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::extended_asm::add_output_operand, in
   jit-recording.c.  */

void
gcc_jit_extended_asm_add_output_operand (gcc_jit_extended_asm *ext_asm,
					 const char *asm_symbolic_name,
					 const char *constraint,
					 gcc_jit_lvalue *dest)
{
  RETURN_IF_FAIL (ext_asm, NULL, NULL, "NULL ext_asm");
  gcc::jit::recording::context *ctxt = ext_asm->get_context ();
  JIT_LOG_FUNC (ctxt->get_logger ());
  gcc::jit::recording::location *loc = ext_asm->get_loc ();
  /* asm_symbolic_name can be NULL.  */
  RETURN_IF_FAIL (constraint, ctxt, loc, "NULL constraint");
  RETURN_IF_FAIL (dest, ctxt, loc, "NULL dest");
  RETURN_IF_FAIL (!ext_asm->is_goto (), ctxt, loc,
		  "cannot add output operand to asm goto");
  ext_asm->add_output_operand (asm_symbolic_name, constraint, dest);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::extended_asm::add_input_operand, in
   jit-recording.c.  */

extern void
gcc_jit_extended_asm_add_input_operand (gcc_jit_extended_asm *ext_asm,
					const char *asm_symbolic_name,
					const char *constraint,
					gcc_jit_rvalue *src)
{
  RETURN_IF_FAIL (ext_asm, NULL, NULL, "NULL ext_asm");
  gcc::jit::recording::context *ctxt = ext_asm->get_context ();
  JIT_LOG_FUNC (ctxt->get_logger ());
  gcc::jit::recording::location *loc = ext_asm->get_loc ();
  /* asm_symbolic_name can be NULL.  */
  RETURN_IF_FAIL (constraint, ctxt, loc, "NULL constraint");
  RETURN_IF_FAIL (src, ctxt, loc, "NULL src");
  ext_asm->add_input_operand (asm_symbolic_name, constraint, src);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::extended_asm::add_clobber, in
   jit-recording.c.  */

void
gcc_jit_extended_asm_add_clobber (gcc_jit_extended_asm *ext_asm,
				  const char *victim)
{
  RETURN_IF_FAIL (ext_asm, NULL, NULL, "NULL ext_asm");
  gcc::jit::recording::context *ctxt = ext_asm->get_context ();
  JIT_LOG_FUNC (ctxt->get_logger ());
  gcc::jit::recording::location *loc = ext_asm->get_loc ();
  RETURN_IF_FAIL (victim, ctxt, loc, "NULL victim");
  ext_asm->add_clobber (victim);
}

/* Public entrypoint.  See description in libgccjit.h.

   After error-checking, the real work is done by the
   gcc::jit::recording::context::add_top_level_asm, in
   jit-recording.c.  */

void
gcc_jit_context_add_top_level_asm (gcc_jit_context *ctxt,
				   gcc_jit_location *loc,
				   const char *asm_stmts)
{
  RETURN_IF_FAIL (ctxt, NULL, NULL, "NULL ctxt");
  JIT_LOG_FUNC (ctxt->get_logger ());
  /* LOC can be NULL.  */
  RETURN_IF_FAIL (asm_stmts, ctxt, NULL, "NULL asm_stmts");
  ctxt->add_top_level_asm (loc, asm_stmts);
}
