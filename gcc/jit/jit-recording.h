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

#ifndef JIT_RECORDING_H
#define JIT_RECORDING_H

#include "jit-common.h"
#include "jit-logging.h"
#include "libgccjit.h"

#include <string>
#include <vector>

#include <unordered_map>

class timer;

extern std::unordered_map<std::string, gcc::jit::recording::function_type*>
  target_function_types;

namespace gcc {

namespace jit {
extern const char * const unary_op_reproducer_strings[];
extern const char * const binary_op_reproducer_strings[];

class result;
class dump;
class reproducer;

/**********************************************************************
 Recording.
 **********************************************************************/

namespace recording {

enum type_info_type {
    TYPE_INFO_ALIGN_OF,
    TYPE_INFO_SIZE_OF,
};

playback::location *
playback_location (replayer *r, location *loc);

const char *
playback_string (string *str);

playback::block *
playback_block (block *b);

/* A recording of a call to gcc_jit_context_enable_dump.  */
struct requested_dump
{
  const char *m_dumpname;
  char **m_out_ptr;
};

/* A JIT-compilation context.  */
class context : public log_user
{
public:
  context (context *parent_ctxt);
  ~context ();

  builtins_manager *
  get_builtins_manager ();

  void record (memento *m);
  void replay_into (replayer *r);
  void disassociate_from_playback ();

  string *
  new_string (const char *text, bool escaped = false);

  location *
  new_location (const char *filename,
		int line,
		int column,
		bool created_by_user);

  type *
  get_type (enum gcc_jit_types type);

  type *
  get_int_type (int num_bytes, int is_signed);

  type *
  new_array_type (location *loc,
		  type *element_type,
		  int num_elements);

  field *
  new_field (location *loc,
	     type *type,
	     const char *name);

  field *
  new_bitfield (location *loc,
                type *type,
                int width,
                const char *name);

  struct_ *
  new_struct_type (location *loc,
		   const char *name);

  union_ *
  new_union_type (location *loc,
		  const char *name);

  function_type *
  new_function_type (type *return_type,
		     int num_params,
		     type **param_types,
		     int is_variadic,
		     bool is_target_builtin);

  type *
  new_function_ptr_type (location *loc,
			 type *return_type,
			 int num_params,
			 type **param_types,
			 int is_variadic);

  param *
  new_param (location *loc,
	     type *type,
	     const char *name);

  function *
  new_function (location *loc,
		enum gcc_jit_function_kind kind,
		type *return_type,
		const char *name,
		int num_params,
		param **params,
		int is_variadic,
		enum built_in_function builtin_id);

  function *
  get_builtin_function (const char *name);

  function *
  get_target_builtin_function (const char *name);

  lvalue *
  new_global (location *loc,
	      enum gcc_jit_global_kind kind,
	      type *type,
	      const char *name);

  rvalue *
  new_ctor (location *loc,
	    type *type,
	    size_t num_values,
	    field **fields,
	    rvalue **values);

  void
  new_global_init_rvalue (lvalue *variable,
			  rvalue *init);

  template <typename HOST_TYPE>
  rvalue *
  new_rvalue_from_const (type *type,
			 HOST_TYPE value);

  rvalue *
  new_sizeof (type *type);

  rvalue *
  new_alignof (type *type);

  rvalue *
  new_string_literal (const char *value);

  rvalue *
  new_rvalue_from_vector (location *loc,
			  vector_type *type,
			  rvalue **elements);

  rvalue *
  new_rvalue_vector_perm (location *loc,
			  rvalue *elements1,
			  rvalue *elements2,
			  rvalue *mask);

  rvalue *
  new_unary_op (location *loc,
		enum gcc_jit_unary_op op,
		type *result_type,
		rvalue *a);

  rvalue *
  new_binary_op (location *loc,
		 enum gcc_jit_binary_op op,
		 type *result_type,
		 rvalue *a, rvalue *b);

  rvalue *
  new_comparison (location *loc,
		  enum gcc_jit_comparison op,
		  rvalue *a, rvalue *b);

  rvalue *
  new_call (location *loc,
	    function *func,
	    int numargs, rvalue **args);

  rvalue *
  new_call_through_ptr (location *loc,
			rvalue *fn_ptr,
			int numargs, rvalue **args);

  rvalue *
  new_cast (location *loc,
	    rvalue *expr,
	    type *type_);

  rvalue *
  new_bitcast (location *loc,
	       rvalue *expr,
	       type *type_);

  lvalue *
  new_array_access (location *loc,
		    rvalue *ptr,
		    rvalue *index);

  rvalue *
  new_convert_vector (location *loc,
		  rvalue *vector,
		  type *type);

  lvalue *
  new_vector_access (location *loc,
		     rvalue *vector,
		     rvalue *index);

  case_ *
  new_case (rvalue *min_value,
	    rvalue *max_value,
	    block *block);

  void
  set_str_option (enum gcc_jit_str_option opt,
		  const char *value);

  const char*
  get_str_option (enum gcc_jit_str_option opt);

  void
  set_output_ident (const char *output_ident);

  void
  set_int_option (enum gcc_jit_int_option opt,
		  int value);

  void
  set_bool_option (enum gcc_jit_bool_option opt,
		   int value);

  void
  set_inner_bool_option (enum inner_bool_option inner_opt,
			 int value);

  void
  add_command_line_option (const char *optname);

  void
  append_command_line_options (vec <char *> *argvec);

  void
  add_driver_option (const char *optname);

  void
  append_driver_options (auto_string_vec *argvec);

  void
  enable_dump (const char *dumpname,
	       char **out_ptr);

  const char *
  get_str_option (enum gcc_jit_str_option opt) const
  {
    return m_str_options[opt];
  }

  int
  get_int_option (enum gcc_jit_int_option opt) const
  {
    return m_int_options[opt];
  }

  int
  get_bool_option (enum gcc_jit_bool_option opt) const
  {
    return m_bool_options[opt];
  }

  int
  get_inner_bool_option (enum inner_bool_option opt) const
  {
    return m_inner_bool_options[opt];
  }

  result *
  compile ();

  void
  compile_to_file (enum gcc_jit_output_kind output_kind,
		   const char *output_path);

  void
  add_error (location *loc, const char *fmt, ...)
      GNU_PRINTF(3, 4);

  void
  add_error_va (location *loc, const char *fmt, va_list ap)
      GNU_PRINTF(3, 0);

  const char *
  get_first_error () const;

  const char *
  get_last_error () const;

  bool errors_occurred () const
  {
    if (m_parent_ctxt)
      if (m_parent_ctxt->errors_occurred ())
	return true;
    return m_error_count;
  }

  type *get_opaque_FILE_type ();

  void dump_to_file (const char *path, bool update_locations);

  void dump_reproducer_to_file (const char *path);

  void
  get_all_requested_dumps (vec <recording::requested_dump> *out);

  void set_timer (timer *t) { m_timer = t; }
  timer *get_timer () const { return m_timer; }

  void add_top_level_asm (location *loc, const char *asm_stmts);

private:
  void log_all_options () const;
  void log_str_option (enum gcc_jit_str_option opt) const;
  void log_int_option (enum gcc_jit_int_option opt) const;
  void log_bool_option (enum gcc_jit_bool_option opt) const;
  void log_inner_bool_option (enum inner_bool_option opt) const;

  void validate ();

private:
  context *m_parent_ctxt;

  /* The ultimate ancestor of the contexts within a family tree of
     contexts.  This has itself as its own m_toplevel_ctxt.  */
  context *m_toplevel_ctxt;

  timer *m_timer;

  int m_error_count;

  char *m_first_error_str;
  bool m_owns_first_error_str;

  char *m_last_error_str;
  bool m_owns_last_error_str;

  char *m_str_options[GCC_JIT_NUM_STR_OPTIONS];
  int m_int_options[GCC_JIT_NUM_INT_OPTIONS];
  bool m_bool_options[GCC_JIT_NUM_BOOL_OPTIONS];
  bool m_inner_bool_options[NUM_INNER_BOOL_OPTIONS];
  auto_vec <char *> m_command_line_options;
  auto_vec <char *> m_driver_options;

  /* Dumpfiles that were requested via gcc_jit_context_enable_dump.  */
  auto_vec<requested_dump> m_requested_dumps;

  /* Recorded API usage.  */
  auto_vec<memento *> m_mementos;

  /* Specific recordings, for use by dump_to_file.  */
  auto_vec<compound_type *> m_compound_types;
  auto_vec<global *> m_globals;
  auto_vec<function *> m_functions;
  auto_vec<top_level_asm *> m_top_level_asms;

  type *m_basic_types[NUM_GCC_JIT_TYPES];
  type *m_FILE_type;

  builtins_manager *m_builtins_manager; // lazily created
};


/* An object with lifetime managed by the context i.e.
   it lives until the context is released, at which
   point it itself is cleaned up.  */

class memento
{
public:
  virtual ~memento () {}

  /* Hook for replaying this.  */
  virtual void replay_into (replayer *r) = 0;

  void set_playback_obj (void *obj) { m_playback_obj = obj; }


  /* Get the context that owns this object.

     Implements the post-error-checking part of
     gcc_jit_object_get_context.  */
  context *get_context () { return m_ctxt; }

  memento *
  as_object () { return this; }

  /* Debugging hook, for use in generating error messages etc.
     Implements the post-error-checking part of
     gcc_jit_object_get_debug_string.  */
  const char *
  get_debug_string ();

  virtual void write_to_dump (dump &d);
  virtual void write_reproducer (reproducer &r) = 0;
  virtual location *dyn_cast_location () { return NULL; }

  memento (const memento&) = delete;
  memento& operator= (const memento&) = delete;

protected:
  memento (context *ctxt)
  : m_ctxt (ctxt),
    m_playback_obj (NULL),
    m_debug_string (NULL)
  {
    gcc_assert (ctxt);
  }

  string *new_string (const char *text) { return m_ctxt->new_string (text); }

private:
  virtual string * make_debug_string () = 0;

public:
  context *m_ctxt;

protected:
  void *m_playback_obj;

private:
  string *m_debug_string;
};

/* or just use std::string? */
class string : public memento
{
public:
  string (context *ctxt, const char *text, bool escaped);
  ~string ();

  const char *c_str () const { return m_buffer; }

  static string * from_printf (context *ctxt, const char *fmt, ...)
    GNU_PRINTF(2, 3);

  void replay_into (replayer *) final override {}

  string (const string&) = delete;
  string& operator= (const string&) = delete;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  size_t m_len;
  char *m_buffer;

  /* Flag to track if this string is the result of string::make_debug_string,
     to avoid infinite recursion when logging all mementos: don't re-escape
     such strings.  */
  bool m_escaped;
};

class output_ident : public memento
{
public:
  output_ident (context *ctxt, const char *text);
  ~output_ident ();

  void replay_into (replayer *) final override;

  output_ident (const output_ident&) = delete;
  output_ident& operator= (const output_ident&) = delete;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  char *m_ident;
};

class location : public memento
{
public:
  location (context *ctxt, string *filename, int line, int column,
	    bool created_by_user)
  : memento (ctxt),
    m_filename (filename),
    m_line (line),
    m_column (column),
    m_created_by_user (created_by_user)
 {}

  void replay_into (replayer *r) final override;

  playback::location *
  playback_location (replayer *r)
  {
    /* Normally during playback, we can walk forwards through the list of
       recording objects, playing them back.  The ordering of recording
       ensures that everything that a recording object refers to has
       already been played back, so we can simply look up the relevant
       m_playback_obj.

       Locations are an exception, due to the "write_to_dump" method of
       recording::statement.  This method can set a new location on a
       statement after the statement is created, and thus the location
       appears in the context's memento list *after* the statement that
       refers to it.

       In such circumstances, the statement is replayed *before* the location,
       when the latter doesn't yet have a playback object.

       Hence we need to ensure that locations have playback objects.  */
    if (!m_playback_obj)
      {
	replay_into (r);
      }
    gcc_assert (m_playback_obj);
    return static_cast <playback::location *> (m_playback_obj);
  }

  location *dyn_cast_location () final override { return this; }
  bool created_by_user () const { return m_created_by_user; }

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  string *m_filename;
  int m_line;
  int m_column;
  bool m_created_by_user;
};

class type : public memento
{
public:
  type *get_pointer ();
  type *get_const ();
  type *get_volatile ();
  type *get_restrict ();
  type *get_aligned (size_t alignment_in_bytes);
  type *get_vector (size_t num_units);

  /* Get the type obtained when dereferencing this type.

     This will return NULL if it's not valid to dereference this type.
     The caller is responsible for setting an error.  */
  virtual type *dereference () = 0;
  /* Get the type size in bytes.

     This is implemented only for memento_of_get_type and
     memento_of_get_pointer as it is used for initializing globals of
     these types.  */
  virtual size_t get_size () { gcc_unreachable (); }

  virtual type* copy (context* ctxt) = 0;

  /* Dynamic casts.  */
  virtual function_type *dyn_cast_function_type () { return NULL; }
  virtual function_type *as_a_function_type() { gcc_unreachable (); return NULL; }
  virtual struct_ *dyn_cast_struct () { return NULL; }
  virtual vector_type *dyn_cast_vector_type () { return NULL; }
  virtual array_type *dyn_cast_array_type () { return NULL; }
  virtual memento_of_get_aligned *dyn_cast_aligned_type () { return NULL; }

  /* Is it typesafe to copy to this type from rtype?  */
  virtual bool accepts_writes_from (type *rtype)
  {
    gcc_assert (rtype);
    return this->unqualified ()->is_same_type_as (rtype->unqualified ());
  }

  virtual bool is_same_type_as (type *other)
  {
    if (is_int ()
		 && other->is_int ()
		 && get_size () == other->get_size ()
		 && is_signed () == other->is_signed ())
    {
      /* LHS (this) is an integer of the same size and sign as rtype.  */
      return true;
    }
    return this == other;
  }

  /* Strip off "const" etc */
  virtual type *unqualified ()
  {
    return this;
  }

  virtual bool is_int () const = 0;
  virtual bool is_float () const = 0;
  virtual bool is_bool () const = 0;
  virtual bool is_numeric_vector () const { return false; }
  virtual type *is_pointer () = 0;
  virtual type *is_volatile () { return NULL; }
  virtual type *is_restrict () { return NULL; }
  virtual type *is_const () { return NULL; }
  virtual type *is_aligned () { return NULL; }
  virtual type *is_array () = 0;
  virtual struct_ *is_struct () { return NULL; }
  virtual bool is_union () const { return false; }
  virtual bool is_void () const { return false; }
  virtual vector_type *is_vector () { return NULL; }
  virtual bool has_known_size () const { return true; }
  virtual bool is_signed () const = 0;

  bool is_numeric () const
  {
    return is_int () || is_float () || is_bool ();
  }

  playback::type *
  playback_type ()
  {
    return static_cast <playback::type *> (m_playback_obj);
  }

  virtual const char *access_as_type (reproducer &r);

protected:
  type (context *ctxt)
    : memento (ctxt),
    m_pointer_to_this_type (NULL)
  {}

private:
  type *m_pointer_to_this_type;
};

/* Result of "gcc_jit_context_get_type".  */
class memento_of_get_type : public type
{
public:
  memento_of_get_type (context *ctxt,
		       enum gcc_jit_types kind)
  : type (ctxt),
    m_kind (kind) {}

  type *dereference () final override;

  size_t get_size () final override;

  type* copy (context* ctxt) final override
  {
    return ctxt->get_type (m_kind);
  }

  bool accepts_writes_from (type *rtype) final override
  {
    if (m_kind == GCC_JIT_TYPE_VOID_PTR)
      {
	if (rtype->is_pointer ())
	  {
	    /* LHS (this) is type (void *), and the RHS is a pointer:
	       accept it:  */
	    return true;
	  }
      }

    return type::accepts_writes_from (rtype);
  }

  bool is_int () const final override;
  bool is_float () const final override;
  bool is_bool () const final override;
  type *is_pointer () final override { return dereference (); }
  type *is_array () final override { return NULL; }
  bool is_void () const final override { return m_kind == GCC_JIT_TYPE_VOID; }
  bool is_signed () const final override;

public:
  void replay_into (replayer *r) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  enum gcc_jit_types m_kind;
};

/* Result of "gcc_jit_type_get_pointer".  */
class memento_of_get_pointer : public type
{
public:
  memento_of_get_pointer (type *other_type)
  : type (other_type->m_ctxt),
    m_other_type (other_type) {}

  type *dereference () final override { return m_other_type; }

  type* copy (context* ctxt) final override
  {
    type* result = new memento_of_get_pointer (m_other_type->copy (ctxt));
    ctxt->record (result);
    return result;
  }

  size_t get_size () final override;

  bool accepts_writes_from (type *rtype) final override;

  void replay_into (replayer *r) final override;

  bool is_int () const final override { return false; }
  bool is_float () const final override { return false; }
  bool is_bool () const final override { return false; }
  type *is_pointer () final override { return m_other_type; }
  type *is_array () final override { return NULL; }
  bool is_signed () const final override { return false; }

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  type *m_other_type;
};

/* A decorated version of a type, for get_const, get_volatile,
   get_aligned, get_restrict, and get_vector.  */

class decorated_type : public type
{
public:
  decorated_type (type *other_type)
  : type (other_type->m_ctxt),
    m_other_type (other_type) {}

  type *dereference () final override { return m_other_type->dereference (); }

  size_t get_size () final override { return m_other_type->get_size (); };

  bool is_int () const override { return m_other_type->is_int (); }
  bool is_float () const final override { return m_other_type->is_float (); }
  bool is_bool () const final override { return m_other_type->is_bool (); }
  bool is_numeric_vector () const override {
      return m_other_type->is_numeric_vector ();
  }
  type *is_pointer () final override { return m_other_type->is_pointer (); }
  type *is_array () final override { return m_other_type->is_array (); }
  struct_ *is_struct () final override { return m_other_type->is_struct (); }
  bool is_signed () const final override { return m_other_type->is_signed (); }

protected:
  type *m_other_type;
};

/* Result of "gcc_jit_type_get_const".  */
class memento_of_get_const : public decorated_type
{
public:
  memento_of_get_const (type *other_type)
  : decorated_type (other_type) {}

  bool accepts_writes_from (type */*rtype*/) final override
  {
    /* Can't write to a "const".  */
    return false;
  }

  type* copy (context* ctxt) final override
  {
    type* result = new memento_of_get_const (m_other_type->copy (ctxt));
    ctxt->record (result);
    return result;
  }

  /* Strip off the "const", giving the underlying type.  */
  type *unqualified () final override { return m_other_type; }

  bool is_same_type_as (type *other) final override
  {
    if (!other->is_const ())
      return false;
    return m_other_type->is_same_type_as (other->is_const ());
  }

  type *is_const () final override { return m_other_type; }

  void replay_into (replayer *) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
};

/* Result of "gcc_jit_type_get_volatile".  */
class memento_of_get_volatile : public decorated_type
{
public:
  memento_of_get_volatile (type *other_type)
  : decorated_type (other_type) {}

  bool is_same_type_as (type *other) final override
  {
    if (!other->is_volatile ())
      return false;
    return m_other_type->is_same_type_as (other->is_volatile ());
  }

  type* copy (context* ctxt) final override
  {
    type* result = new memento_of_get_volatile (m_other_type->copy (ctxt));
    ctxt->record (result);
    return result;
  }

  /* Strip off the "volatile", giving the underlying type.  */
  type *unqualified () final override { return m_other_type; }

  type *is_volatile () final override { return m_other_type; }

  void replay_into (replayer *) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
};

/* Result of "gcc_jit_type_get_restrict".  */
class memento_of_get_restrict : public decorated_type
{
public:
  memento_of_get_restrict (type *other_type)
  : decorated_type (other_type) {}

  bool is_same_type_as (type *other) final override
  {
    if (!other->is_restrict ())
      return false;
    return m_other_type->is_same_type_as (other->is_restrict ());
  }

  type* copy (context* ctxt) final override
  {
    type* result = new memento_of_get_restrict (m_other_type->copy (ctxt));
    ctxt->record (result);
    return result;
  }

  /* Strip off the "restrict", giving the underlying type.  */
  type *unqualified () final override { return m_other_type; }

  type *is_restrict () final override { return m_other_type; }

  void replay_into (replayer *) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
};

/* Result of "gcc_jit_type_get_aligned".  */
class memento_of_get_aligned : public decorated_type
{
public:
  memento_of_get_aligned (type *other_type, size_t alignment_in_bytes)
  : decorated_type (other_type),
    m_alignment_in_bytes (alignment_in_bytes) {}

  bool is_same_type_as (type *other) final override
  {
    if (!other->is_aligned ())
    {
      return m_other_type->is_same_type_as (other);
    }
    return m_alignment_in_bytes
	== other->dyn_cast_aligned_type ()->m_alignment_in_bytes
	&& m_other_type->is_same_type_as (other->is_aligned ());
  }

  type *is_aligned () final override { return m_other_type; }

  type* copy (context* ctxt) final override
  {
    type* result = new memento_of_get_aligned (m_other_type->copy (ctxt),
					       m_alignment_in_bytes);
    ctxt->record (result);
    return result;
  }

  /* Strip off the alignment, giving the underlying type.  */
  type *unqualified () final override { return m_other_type; }

  void replay_into (replayer *) final override;
  memento_of_get_aligned *dyn_cast_aligned_type () final override 
  {
    return this;
  }

  array_type *dyn_cast_array_type () final override
  {
    return m_other_type->dyn_cast_array_type ();
  }

  vector_type *dyn_cast_vector_type () final override {
    return m_other_type->dyn_cast_vector_type ();
  }

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  size_t m_alignment_in_bytes;
};

/* Result of "gcc_jit_type_get_vector".  */
class vector_type : public decorated_type
{
public:
  vector_type (type *other_type, size_t num_units)
  : decorated_type (other_type),
    m_num_units (num_units) {}

  bool is_int () const final override {
    return false;
  }

  bool is_numeric_vector () const final override {
    return true;
  }

  type* copy (context* ctxt) final override
  {
    type* result = new vector_type (m_other_type->copy (ctxt), m_num_units);
    ctxt->record (result);
    return result;
  }

  size_t get_num_units () const { return m_num_units; }

  vector_type *dyn_cast_vector_type () final override { return this; }

  type *get_element_type () { return m_other_type; }

  void replay_into (replayer *) final override;

  bool is_same_type_as (type *other) final override
  {
    vector_type *other_vec_type = other->dyn_cast_vector_type ();
    if (other_vec_type == NULL)
      return false;
    return get_num_units () == other_vec_type->get_num_units ()
      && get_element_type () == other_vec_type->get_element_type ();
  }

  vector_type *is_vector () final override { return this; }

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  size_t m_num_units;
};

class array_type : public type
{
 public:
  array_type (context *ctxt,
	      location *loc,
	      type *element_type,
	      int num_elements)
  : type (ctxt),
    m_loc (loc),
    m_element_type (element_type),
    m_num_elements (num_elements)
  {}

  type *dereference () final override;

  bool is_same_type_as (type *other) final override
  {
    array_type *other_array_type = other->dyn_cast_array_type ();
    if (!other_array_type)
      return false;
    return m_num_elements == other_array_type->m_num_elements
      && m_element_type->is_same_type_as (other_array_type->m_element_type);
  }

  array_type *dyn_cast_array_type () final override { return this; }

  type* copy (context* ctxt) final override
  {
    type* result = new array_type (ctxt, m_loc, m_element_type->copy (ctxt),
				   m_num_elements);
    ctxt->record (result);
    return result;
  }

  bool is_int () const final override { return false; }
  bool is_float () const final override { return false; }
  bool is_bool () const final override { return false; }
  type *is_pointer () final override { return NULL; }
  type *is_array () final override { return m_element_type; }
  int num_elements () { return m_num_elements; }
  bool is_signed () const final override { return false; }

  void replay_into (replayer *) final override;

 private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

 private:
  location *m_loc;
  type *m_element_type;
  int m_num_elements;
};

class function_type : public type
{
public:
  function_type (context *ctxt,
		 type *return_type,
		 int num_params,
		 type **param_types,
		 int is_variadic,
		 bool is_target_builtin);

  type *dereference () final override;
  function_type *dyn_cast_function_type () final override { return this; }
  function_type *as_a_function_type () final override { return this; }

  bool is_same_type_as (type *other) final override;

  type* copy (context* ctxt) final override
  {
    auto_vec<type *> new_params{};
    for (size_t i = 0; i < m_param_types.length (); i++)
      new_params.safe_push (m_param_types[i]->copy (ctxt));

    type* result = new function_type (ctxt, m_return_type->copy (ctxt),
				      m_param_types.length (),
				      new_params.address (),
				      m_is_variadic, m_is_target_builtin);
    ctxt->record (result);
    return result;
  }

  bool is_int () const final override { return false; }
  bool is_float () const final override { return false; }
  bool is_bool () const final override { return false; }
  type *is_pointer () final override { return NULL; }
  type *is_array () final override { return NULL; }
  bool is_signed () const final override { return false; }

  void replay_into (replayer *) final override;

  type * get_return_type () const { return m_return_type; }
  const vec<type *> &get_param_types () const { return m_param_types; }
  int is_variadic () const { return m_is_variadic; }

  string * make_debug_string_with_ptr ();

  void
  write_deferred_reproducer (reproducer &r,
			     memento *ptr_type);

 private:
  string * make_debug_string () final override;
  string * make_debug_string_with (const char *);
  void write_reproducer (reproducer &r) final override;

private:
  type *m_return_type;
  auto_vec<type *> m_param_types;
  int m_is_variadic;
  bool m_is_target_builtin;
};

class field : public memento
{
public:
  field (context *ctxt,
	 location *loc,
	 type *type,
	 string *name)
  : memento (ctxt),
    m_loc (loc),
    m_type (type),
    m_name (name),
    m_container (NULL)
  {}

  type * get_type () const { return m_type; }

  compound_type * get_container () const { return m_container; }
  void set_container (compound_type *c) { m_container = c; }

  void replay_into (replayer *) override;

  void write_to_dump (dump &d) override;

  playback::field *
  playback_field () const
  {
    return static_cast <playback::field *> (m_playback_obj);
  }

private:
  string * make_debug_string () override;
  void write_reproducer (reproducer &r) override;

protected:
  location *m_loc;
  type *m_type;
  string *m_name;
  compound_type *m_container;
};


class bitfield : public field
{
public:
  bitfield (context *ctxt,
	    location *loc,
	    type *type,
	    int width,
	    string *name)
    : field (ctxt, loc, type, name),
      m_width (width)
  {}

  void replay_into (replayer *) final override;

  void write_to_dump (dump &d) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  int m_width;
};

/* Base class for struct_ and union_ */
class compound_type : public type
{
public:
  compound_type (context *ctxt,
		 location *loc,
		 string *name);

  string *get_name () const { return m_name; }
  location *get_loc () const { return m_loc; }
  fields * get_fields () { return m_fields; }

  void
  set_fields (location *loc,
	      int num_fields,
	      field **fields);

  type *dereference () final override;

  bool is_int () const final override { return false; }
  bool is_float () const final override { return false; }
  bool is_bool () const final override { return false; }
  type *is_pointer () final override { return NULL; }
  type *is_array () final override { return NULL; }
  bool is_signed () const final override { return false; }

  bool has_known_size () const final override { return m_fields != NULL; }

  playback::compound_type *
  playback_compound_type ()
  {
    return static_cast <playback::compound_type *> (m_playback_obj);
  }

protected:
  location *m_loc;
  string *m_name;

private:
  fields *m_fields;
};

class struct_ : public compound_type
{
public:
  struct_ (context *ctxt,
	   location *loc,
	   string *name);

  struct_ *dyn_cast_struct () final override { return this; }

  type* copy (context* ctxt) final override
  {
    type* result = new struct_ (ctxt, m_loc, m_name);
    ctxt->record (result);
    return result;
  }

  type *
  as_type () { return this; }

  void replay_into (replayer *r) final override;

  const char *access_as_type (reproducer &r) final override;

  struct_ *is_struct () final override { return this; }

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
};

// memento of struct_::set_fields
class fields : public memento
{
public:
  fields (compound_type *struct_or_union,
	  int num_fields,
	  field **fields);

  void replay_into (replayer *r) final override;

  void write_to_dump (dump &d) final override;

  int length () const { return m_fields.length (); }
  field *get_field (int i) const { return m_fields[i]; }

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  compound_type *m_struct_or_union;
  auto_vec<field *> m_fields;
};

class union_ : public compound_type
{
public:
  union_ (context *ctxt,
	  location *loc,
	  string *name);

  void replay_into (replayer *r) final override;

  type* copy (context* ctxt) final override
  {
    type* result = new union_ (ctxt, m_loc, m_name);
    ctxt->record (result);
    return result;
  }

  bool is_union () const final override { return true; }

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
};

/* An abstract base class for operations that visit all rvalues within an
   expression tree.
   Currently the only implementation is class rvalue_usage_validator within
   jit-recording.cc.  */

class rvalue_visitor
{
 public:
  virtual ~rvalue_visitor () {}
  virtual void visit (rvalue *rvalue) = 0;
};

/* When generating debug strings for rvalues we mimic C, so we need to
   mimic C's precedence levels when handling compound expressions.
   These are in order from strongest precedence to weakest.  */
enum precedence
{
  PRECEDENCE_PRIMARY,
  PRECEDENCE_POSTFIX,
  PRECEDENCE_UNARY,
  PRECEDENCE_CAST,
  PRECEDENCE_MULTIPLICATIVE,
  PRECEDENCE_ADDITIVE,
  PRECEDENCE_SHIFT,
  PRECEDENCE_RELATIONAL,
  PRECEDENCE_EQUALITY,
  PRECEDENCE_BITWISE_AND,
  PRECEDENCE_BITWISE_XOR,
  PRECEDENCE_BITWISE_IOR,
  PRECEDENCE_LOGICAL_AND,
  PRECEDENCE_LOGICAL_OR
};

class rvalue : public memento
{
public:
  rvalue (context *ctxt,
	  location *loc,
	  type *type_)
  : memento (ctxt),
    m_loc (loc),
    m_type (type_),
    m_scope (NULL),
    m_parenthesized_string (NULL)
  {
    gcc_assert (type_);
  }

  location * get_loc () const { return m_loc; }

  /* Get the recording::type of this rvalue.

     Implements the post-error-checking part of
     gcc_jit_rvalue_get_type.  */
  type * get_type () const { return m_type; }

  playback::rvalue *
  playback_rvalue () const
  {
    return static_cast <playback::rvalue *> (m_playback_obj);
  }
  rvalue *
  access_field (location *loc,
		field *field);

  lvalue *
  dereference_field (location *loc,
		     field *field);

  lvalue *
  dereference (location *loc);

  void
  verify_valid_within_stmt (const char *api_funcname, statement *s);

  virtual void visit_children (rvalue_visitor *v) = 0;

  void set_scope (function *scope);
  function *get_scope () const { return m_scope; }

  /* Dynamic casts.  */
  virtual param *dyn_cast_param () { return NULL; }
  virtual base_call *dyn_cast_base_call () { return NULL; }

  virtual const char *access_as_rvalue (reproducer &r);

  /* Get the debug string, wrapped in parentheses.  */
  const char *
  get_debug_string_parens (enum precedence outer_prec);

  virtual bool is_constant () const { return false; }
  virtual bool get_wide_int (wide_int *) const { return false; }

private:
  virtual enum precedence get_precedence () const = 0;

protected:
  location *m_loc;
  type *m_type;

 private:
  function *m_scope; /* NULL for globals, non-NULL for locals/params */
  string *m_parenthesized_string;
};

class lvalue : public rvalue
{
public:
  lvalue (context *ctxt,
	  location *loc,
	  type *type_)
  : rvalue (ctxt, loc, type_),
    m_link_section (NULL),
    m_reg_name (NULL),
    m_tls_model (GCC_JIT_TLS_MODEL_NONE),
    m_alignment (0),
    m_string_attributes ()
  {}

  playback::lvalue *
  playback_lvalue () const
  {
    return static_cast <playback::lvalue *> (m_playback_obj);
  }

  lvalue *
  access_field (location *loc,
		field *field);

  rvalue *
  get_address (location *loc);

  rvalue *
  as_rvalue () { return this; }

  const char *access_as_rvalue (reproducer &r) override;

  void add_string_attribute (gcc_jit_variable_attribute attribute, const char* value);

  bool get_readonly () const
  {
    return m_readonly;
  }

  void set_readonly ()
  {
    m_readonly = true;
  }

  virtual const char *access_as_lvalue (reproducer &r);
  virtual bool is_global () const { return false; }
  virtual bool is_local () const { return false; }
  void set_tls_model (enum gcc_jit_tls_model model);
  void set_link_section (const char *name);
  void set_register_name (const char *reg_name);
  void set_alignment (unsigned bytes);
  unsigned get_alignment () const { return m_alignment; }

protected:
  string *m_link_section;
  string *m_reg_name;
  enum gcc_jit_tls_model m_tls_model;
  unsigned m_alignment;
  std::vector<std::pair<gcc_jit_variable_attribute,
	      std::string>> m_string_attributes;
  bool m_readonly = false;
};

class param : public lvalue
{
public:
  param (context *ctxt,
	 location *loc,
	 type *type,
	 string *name)
    : lvalue (ctxt, loc, type),
    m_name (name) {}

  lvalue *
  as_lvalue () { return this; }

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *) final override {}

  playback::param *
  playback_param () const
  {
    return static_cast <playback::param *> (m_playback_obj);
  }

  param *dyn_cast_param () final override { return this; }

  const char *access_as_rvalue (reproducer &r) final override;
  const char *access_as_lvalue (reproducer &r) final override;

private:
  string * make_debug_string () final override { return m_name; }
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_PRIMARY;
  }

private:
  string *m_name;
};

class function : public memento
{
public:
  function (context *ctxt,
	    location *loc,
	    enum gcc_jit_function_kind kind,
	    type *return_type,
	    string *name,
	    int num_params,
	    param **params,
	    int is_variadic,
	    enum built_in_function builtin_id,
	    bool is_target_builtin);

  void replay_into (replayer *r) final override;

  playback::function *
  playback_function () const
  {
    return static_cast <playback::function *> (m_playback_obj);
  }

  enum gcc_jit_function_kind get_kind () const { return m_kind; }

  lvalue *
  new_local (location *loc,
	     type *type,
	     const char *name);

  lvalue *
  new_temp (location *loc,
	    type *type);

  block*
  new_block (const char *name);

  location *get_loc () const { return m_loc; }
  type *get_return_type () const { return m_return_type; }
  string * get_name () const { return m_name; }
  const vec<param *> &get_params () const { return m_params; }

  /* Get the given param by index.
     Implements the post-error-checking part of
     gcc_jit_function_get_param.  */
  param *get_param (int i) const { return m_params[i]; }

  bool is_variadic () const { return m_is_variadic; }

  void write_to_dump (dump &d) final override;

  void validate ();

  void dump_to_dot (const char *path);

  rvalue *get_address (location *loc);

  void add_attribute (gcc_jit_fn_attribute attribute);
  void add_string_attribute (gcc_jit_fn_attribute attribute, const char* value);
  void add_integer_array_attribute (gcc_jit_fn_attribute attribute, const int* value, size_t length);

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  location *m_loc;
  enum gcc_jit_function_kind m_kind;
  type *m_return_type;
  string *m_name;
  auto_vec<param *> m_params;
  int m_is_variadic;
  enum built_in_function m_builtin_id;
  auto_vec<local *> m_locals;
  auto_vec<block *> m_blocks;
  type *m_fn_ptr_type;
  std::vector<gcc_jit_fn_attribute> m_attributes;
  std::vector<std::pair<gcc_jit_fn_attribute, std::string>> m_string_attributes;
  std::vector<std::pair<gcc_jit_fn_attribute, std::vector<int>>> m_int_array_attributes;
  bool m_is_target_builtin;
};

class block : public memento
{
public:
  block (function *func, int index, string *name)
  : memento (func->m_ctxt),
    m_func (func),
    m_index (index),
    m_name (name),
    m_statements (),
    m_has_been_terminated (false),
    m_is_reachable (false)
  {
  }

  /* Get the recording::function containing this block.
     Implements the post-error-checking part of
     gcc_jit_block_get_function.  */
  function *get_function () { return m_func; }

  bool has_been_terminated () { return m_has_been_terminated; }
  bool is_reachable () { return m_is_reachable; }

  statement *
  add_eval (location *loc,
	    rvalue *rvalue);

  statement *
  add_assignment (location *loc,
		  lvalue *lvalue,
		  rvalue *rvalue);

  statement *
  add_assignment_op (location *loc,
		     lvalue *lvalue,
		     enum gcc_jit_binary_op op,
		     rvalue *rvalue);

  statement *
  add_comment (location *loc,
	       const char *text);

  extended_asm *
  add_extended_asm (location *loc,
		    const char *asm_template);

  statement *
  end_with_conditional (location *loc,
			rvalue *boolval,
			block *on_true,
			block *on_false);

  statement *
  end_with_jump (location *loc,
		 block *target);

  statement *
  end_with_return (location *loc,
		   rvalue *rvalue);

  statement *
  end_with_switch (location *loc,
		   rvalue *expr,
		   block *default_block,
		   int num_cases,
		   case_ **cases);

  extended_asm *
  end_with_extended_asm_goto (location *loc,
			      const char *asm_template,
			      int num_goto_blocks,
			      block **goto_blocks,
			      block *fallthrough_block);

  playback::block *
  playback_block () const
  {
    return static_cast <playback::block *> (m_playback_obj);
  }

  void write_to_dump (dump &d) final override;

  bool validate ();

  location *get_loc () const;

  statement *get_first_statement () const;
  statement *get_last_statement () const;

  vec <block *> get_successor_blocks () const;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

  void replay_into (replayer *r) final override;

  void dump_to_dot (pretty_printer *pp);
  void dump_edges_to_dot (pretty_printer *pp);

private:
  function *m_func;
  int m_index;
  string *m_name;
  auto_vec<statement *> m_statements;
  bool m_has_been_terminated;
  bool m_is_reachable;

  friend class function;
};

class global : public lvalue
{
public:
  global (context *ctxt,
	  location *loc,
	  enum gcc_jit_global_kind kind,
	  type *type,
	  string *name)
  : lvalue (ctxt, loc, type),
    m_kind (kind),
    m_name (name)
  {
    m_initializer = NULL;
    m_initializer_num_bytes = 0;
  }
  ~global ()
  {
    free (m_initializer);
  }

  void replay_into (replayer *) final override;

  void visit_children (rvalue_visitor *) final override {}

  void write_to_dump (dump &d) final override;

  bool is_global () const final override { return true; }

  void
  set_initializer (const void *initializer,
                   size_t num_bytes)
  {
    if (m_initializer)
      free (m_initializer);
    m_initializer = xmalloc (num_bytes);
    memcpy (m_initializer, initializer, num_bytes);
    m_initializer_num_bytes = num_bytes;
  }

  void set_flags (int flag_fields)
  {
    m_flags = (enum global_var_flags)(m_flags | flag_fields);
  }
  /* Returns true if any of the flags in the argument is set.  */
  bool test_flags_anyof (int flag_fields) const
  {
    return m_flags & flag_fields;
  }

  enum gcc_jit_global_kind get_kind () const
  {
    return m_kind;
  }

  void set_rvalue_init (rvalue *val) { m_rvalue_init = val; }

private:
  string * make_debug_string () final override { return m_name; }
  template <typename T>
  void write_initializer_reproducer (const char *id, reproducer &r);
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_PRIMARY;
  }

private:
  enum gcc_jit_global_kind m_kind;
  enum global_var_flags m_flags = GLOBAL_VAR_FLAGS_NONE;
  string *m_name;
  void *m_initializer;
  rvalue *m_rvalue_init = nullptr; /* Only needed for write_dump.  */
  size_t m_initializer_num_bytes;
};

template <typename HOST_TYPE>
class memento_of_new_rvalue_from_const : public rvalue
{
public:
  memento_of_new_rvalue_from_const (context *ctxt,
				    location *loc,
				    type *type,
				    HOST_TYPE value)
  : rvalue (ctxt, loc, type),
    m_value (value) {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *) final override {}

  bool is_constant () const final override { return true; }

  bool get_wide_int (wide_int *out) const final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_PRIMARY;
  }

private:
  HOST_TYPE m_value;
};

class memento_of_typeinfo : public rvalue
{
public:
  memento_of_typeinfo (context *ctxt,
			 location *loc,
			 type *type,
			 type_info_type type_info)
  : rvalue (ctxt, loc, ctxt->get_type (GCC_JIT_TYPE_INT)),
    m_type (type),
    m_info_type (type_info) {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *) final override {}

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_PRIMARY;
  }

private:
  type *m_type;
  type_info_type m_info_type;
};

class memento_of_new_string_literal : public rvalue
{
public:
  memento_of_new_string_literal (context *ctxt,
				 location *loc,
				 string *value)
  : rvalue (ctxt, loc, ctxt->get_type (GCC_JIT_TYPE_CONST_CHAR_PTR)),
    m_value (value) {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *) final override {}

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_PRIMARY;
  }

private:
  string *m_value;
};

class memento_of_new_rvalue_from_vector : public rvalue
{
public:
  memento_of_new_rvalue_from_vector (context *ctxt,
				     location *loc,
				     vector_type *type,
				     rvalue **elements);

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_PRIMARY;
  }

private:
  vector_type *m_vector_type;
  auto_vec<rvalue *> m_elements;
};

class memento_of_new_rvalue_vector_perm : public rvalue
{
public:
  memento_of_new_rvalue_vector_perm (context *ctxt,
				     location *loc,
				     rvalue *elements1,
				     rvalue *elements2,
				     rvalue *mask);

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_PRIMARY;
  }

private:
  rvalue *m_elements1;
  rvalue *m_elements2;
  rvalue *m_mask;
};

class ctor : public rvalue
{
public:
  ctor (context *ctxt,
	location *loc,
	type *type)
  : rvalue (ctxt, loc, type)
  { }

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_PRIMARY;
  }

public:
  auto_vec<field *> m_fields;
  auto_vec<rvalue *> m_values;
};

class unary_op : public rvalue
{
public:
  unary_op (context *ctxt,
	    location *loc,
	    enum gcc_jit_unary_op op,
	    type *result_type,
	    rvalue *a)
  : rvalue (ctxt, loc, result_type),
    m_op (op),
    m_a (a)
  {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_UNARY;
  }

private:
  enum gcc_jit_unary_op m_op;
  rvalue *m_a;
};

class binary_op : public rvalue
{
public:
  binary_op (context *ctxt,
	     location *loc,
	     enum gcc_jit_binary_op op,
	     type *result_type,
	     rvalue *a, rvalue *b)
  : rvalue (ctxt, loc, result_type),
    m_op (op),
    m_a (a),
    m_b (b) {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override;

private:
  enum gcc_jit_binary_op m_op;
  rvalue *m_a;
  rvalue *m_b;
};

class comparison : public rvalue
{
public:
  comparison (context *ctxt,
	      location *loc,
	      enum gcc_jit_comparison op,
	      rvalue *a, rvalue *b)
  : rvalue (ctxt, loc, ctxt->get_type (GCC_JIT_TYPE_BOOL)),
    m_op (op),
    m_a (a),
    m_b (b)
  {
    type *a_type = a->get_type ();
    vector_type *vec_type = a_type->dyn_cast_vector_type ();
    if (vec_type != NULL)
    {
      type *element_type = vec_type->get_element_type ();
      type *inner_type;
      /* Vectors of floating-point values return a vector of integers of the
         same size.  */
      if (element_type->is_float ())
	inner_type = ctxt->get_int_type (element_type->get_size (), false);
      else
	inner_type = element_type;
      m_type = new vector_type (inner_type, vec_type->get_num_units ());
      ctxt->record (m_type);
    }
  }

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override;

private:
  enum gcc_jit_comparison m_op;
  rvalue *m_a;
  rvalue *m_b;
};

class cast : public rvalue
{
public:
  cast (context *ctxt,
	location *loc,
	rvalue *a,
	type *type_)
  : rvalue (ctxt, loc, type_),
    m_rvalue (a)
  {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_CAST;
  }

private:
  rvalue *m_rvalue;
};

class bitcast : public rvalue
{
public:
  bitcast (context *ctxt,
	   location *loc,
	   rvalue *a,
	   type *type_)
  : rvalue (ctxt, loc, type_),
    m_rvalue (a)
  {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_CAST;
  }

private:
  rvalue *m_rvalue;
};

class base_call : public rvalue
{
 public:
  base_call (context *ctxt,
	     location *loc,
	     type *type_,
	     int numargs,
	     rvalue **args);

  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_POSTFIX;
  }

  base_call *dyn_cast_base_call () final override { return this; }

  void set_require_tail_call (bool require_tail_call)
  {
    m_require_tail_call = require_tail_call;
  }

 protected:
  void write_reproducer_tail_call (reproducer &r, const char *id);

 protected:
  auto_vec<rvalue *> m_args;
  bool m_require_tail_call;
};

class call : public base_call
{
public:
  call (context *ctxt,
	location *loc,
	function *func,
	int numargs,
	rvalue **args);

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  function *m_func;
};

class call_through_ptr : public base_call
{
public:
  call_through_ptr (context *ctxt,
		    location *loc,
		    rvalue *fn_ptr,
		    int numargs,
		    rvalue **args);

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  rvalue *m_fn_ptr;
};

class array_access : public lvalue
{
public:
  array_access (context *ctxt,
		location *loc,
		rvalue *ptr,
		rvalue *index)
  : lvalue (ctxt, loc, ptr->get_type ()->dereference ()),
    m_ptr (ptr),
    m_index (index)
  {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_POSTFIX;
  }

private:
  rvalue *m_ptr;
  rvalue *m_index;
};

class convert_vector : public rvalue
{
public:
  convert_vector (context *ctxt,
		  location *loc,
		  rvalue *vector,
		  type *type)
  : rvalue (ctxt, loc, type),
    m_vector (vector),
    m_type (type)
  {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_POSTFIX;
  }

private:
  rvalue *m_vector;
  type *m_type;
};

class vector_access : public lvalue
{
public:
  vector_access (context *ctxt,
		 location *loc,
		 rvalue *vector,
		 rvalue *index)
  : lvalue (ctxt, loc, vector->get_type ()->dyn_cast_vector_type ()
			     ->get_element_type ()),
    m_vector (vector),
    m_index (index)
  {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_POSTFIX;
  }

private:
  rvalue *m_vector;
  rvalue *m_index;
};

class access_field_of_lvalue : public lvalue
{
public:
  access_field_of_lvalue (context *ctxt,
			  location *loc,
			  lvalue *val,
			  field *field)
  : lvalue (ctxt, loc, field->get_type ()),
    m_lvalue (val),
    m_field (field)
  {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_POSTFIX;
  }

private:
  lvalue *m_lvalue;
  field *m_field;
};

class access_field_rvalue : public rvalue
{
public:
  access_field_rvalue (context *ctxt,
		       location *loc,
		       rvalue *val,
		       field *field)
  : rvalue (ctxt, loc, field->get_type ()),
    m_rvalue (val),
    m_field (field)
  {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_POSTFIX;
  }

private:
  rvalue *m_rvalue;
  field *m_field;
};

class dereference_field_rvalue : public lvalue
{
public:
  dereference_field_rvalue (context *ctxt,
			    location *loc,
			    rvalue *val,
			    field *field)
  : lvalue (ctxt, loc, field->get_type ()),
    m_rvalue (val),
    m_field (field)
  {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_POSTFIX;
  }

private:
  rvalue *m_rvalue;
  field *m_field;
};

class dereference_rvalue : public lvalue
{
public:
  dereference_rvalue (context *ctxt,
		      location *loc,
		      rvalue *val)
  : lvalue (ctxt, loc, val->get_type ()->dereference ()),
    m_rvalue (val) {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_UNARY;
  }

private:
  rvalue *m_rvalue;
};

class get_address_of_lvalue : public rvalue
{
public:
  get_address_of_lvalue (context *ctxt,
			 location *loc,
			 lvalue *val)
  : rvalue (ctxt, loc, val->get_type ()->get_pointer ()),
    m_lvalue (val)
  {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_UNARY;
  }

private:
  lvalue *m_lvalue;
};

class function_pointer : public rvalue
{
public:
  function_pointer (context *ctxt,
		    location *loc,
		    function *fn,
		    type *type)
  : rvalue (ctxt, loc, type),
    m_fn (fn) {}

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *v) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_UNARY;
  }

private:
  function *m_fn;
};

class local : public lvalue
{
public:
  local (function *func, location *loc, type *type_, string *name)
    : lvalue (func->m_ctxt, loc, type_),
    m_func (func),
    m_name (name)
  {
    set_scope (func);
  }

  void replay_into (replayer *r) final override;

  void visit_children (rvalue_visitor *) final override {}

  bool is_local () const final override { return true; }

  void write_to_dump (dump &d) final override;

private:
  string * make_debug_string () final override {
    if (m_name)
      return m_name;
    else
      return m_ctxt->new_string ("temp");
  }
  void write_reproducer (reproducer &r) final override;
  enum precedence get_precedence () const final override
  {
    return PRECEDENCE_PRIMARY;
  }

private:
  function *m_func;
  string *m_name;
};

class statement : public memento
{
public:
  virtual vec <block *> get_successor_blocks () const;

  void write_to_dump (dump &d) final override;

  block *get_block () const { return m_block; }
  location *get_loc () const { return m_loc; }

protected:
  statement (block *b, location *loc)
  : memento (b->m_ctxt),
    m_block (b),
    m_loc (loc) {}

  playback::location *
  playback_location (replayer *r) const
  {
    return ::gcc::jit::recording::playback_location (r, m_loc);
  }

private:
  block *m_block;
  location *m_loc;
};

class eval : public statement
{
public:
  eval (block *b,
	location *loc,
	rvalue *rvalue)
  : statement (b, loc),
    m_rvalue (rvalue) {}

  void replay_into (replayer *r) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  rvalue *m_rvalue;
};

class assignment : public statement
{
public:
  assignment (block *b,
	      location *loc,
	      lvalue *lvalue,
	      rvalue *rvalue)
  : statement (b, loc),
    m_lvalue (lvalue),
    m_rvalue (rvalue) {}

  void replay_into (replayer *r) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  lvalue *m_lvalue;
  rvalue *m_rvalue;
};

class assignment_op : public statement
{
public:
  assignment_op (block *b,
		 location *loc,
		 lvalue *lvalue,
		 enum gcc_jit_binary_op op,
		 rvalue *rvalue)
  : statement (b, loc),
    m_lvalue (lvalue),
    m_op (op),
    m_rvalue (rvalue) {}

  void replay_into (replayer *r) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  lvalue *m_lvalue;
  enum gcc_jit_binary_op m_op;
  rvalue *m_rvalue;
};

class comment : public statement
{
public:
  comment (block *b,
	   location *loc,
	   string *text)
  : statement (b, loc),
    m_text (text) {}

  void replay_into (replayer *r) final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  string *m_text;
};

class conditional : public statement
{
public:
  conditional (block *b,
	       location *loc,
	       rvalue *boolval,
	       block *on_true,
	       block *on_false)
  : statement (b, loc),
    m_boolval (boolval),
    m_on_true (on_true),
    m_on_false (on_false) {}

  void replay_into (replayer *r) final override;

  vec <block *> get_successor_blocks () const final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  rvalue *m_boolval;
  block *m_on_true;
  block *m_on_false;
};

class jump : public statement
{
public:
  jump (block *b,
	location *loc,
	block *target)
  : statement (b, loc),
    m_target (target) {}

  void replay_into (replayer *r) final override;

  vec <block *> get_successor_blocks () const final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  block *m_target;
};

class return_ : public statement
{
public:
  return_ (block *b,
	   location *loc,
	   rvalue *rvalue)
  : statement (b, loc),
    m_rvalue (rvalue) {}

  void replay_into (replayer *r) final override;

  vec <block *> get_successor_blocks () const final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  rvalue *m_rvalue;
};

class case_ : public memento
{
 public:
  case_ (context *ctxt,
	 rvalue *min_value,
	 rvalue *max_value,
	 block *dest_block)
  : memento (ctxt),
    m_min_value (min_value),
    m_max_value (max_value),
    m_dest_block (dest_block)
  {}

  rvalue *get_min_value () const { return m_min_value; }
  rvalue *get_max_value () const { return m_max_value; }
  block *get_dest_block () const { return m_dest_block; }

  void replay_into (replayer *) final override { /* empty */ }

  void write_reproducer (reproducer &r) final override;

private:
  string * make_debug_string () final override;

 private:
  rvalue *m_min_value;
  rvalue *m_max_value;
  block *m_dest_block;
};

class switch_ : public statement
{
public:
  switch_ (block *b,
	   location *loc,
	   rvalue *expr,
	   block *default_block,
	   int num_cases,
	   case_ **cases);

  void replay_into (replayer *r) final override;

  vec <block *> get_successor_blocks () const final override;

private:
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  rvalue *m_expr;
  block *m_default_block;
  auto_vec <case_ *> m_cases;
};

class asm_operand : public memento
{
public:
  asm_operand (extended_asm *ext_asm,
	       string *asm_symbolic_name,
	       string *constraint);

  const char *get_symbolic_name () const
  {
    if (m_asm_symbolic_name)
      return m_asm_symbolic_name->c_str ();
    else
      return NULL;
  }

  const char *get_constraint () const
  {
    return m_constraint->c_str ();
  }

  virtual void print (pretty_printer *pp) const;

private:
  string * make_debug_string () final override;

protected:
  extended_asm *m_ext_asm;
  string *m_asm_symbolic_name;
  string *m_constraint;
};

class output_asm_operand : public asm_operand
{
public:
  output_asm_operand (extended_asm *ext_asm,
		      string *asm_symbolic_name,
		      string *constraint,
		      lvalue *dest)
  : asm_operand (ext_asm, asm_symbolic_name, constraint),
    m_dest (dest)
  {}

  lvalue *get_lvalue () const { return m_dest; }

  void replay_into (replayer *) final override {}

  void print (pretty_printer *pp) const final override;

private:
  void write_reproducer (reproducer &r) final override;

private:
  lvalue *m_dest;
};

class input_asm_operand : public asm_operand
{
public:
  input_asm_operand (extended_asm *ext_asm,
		     string *asm_symbolic_name,
		     string *constraint,
		     rvalue *src)
  : asm_operand (ext_asm, asm_symbolic_name, constraint),
    m_src (src)
  {}

  rvalue *get_rvalue () const { return m_src; }

  void replay_into (replayer *) final override {}

  void print (pretty_printer *pp) const final override;

private:
  void write_reproducer (reproducer &r) final override;

private:
  rvalue *m_src;
};

/* Abstract base class for extended_asm statements.  */

class extended_asm : public statement
{
public:
  extended_asm (block *b,
		location *loc,
		string *asm_template)
  : statement (b, loc),
    m_asm_template (asm_template),
    m_is_volatile (false),
    m_is_inline (false)
  {}

  void set_volatile_flag (bool flag) { m_is_volatile = flag; }
  void set_inline_flag (bool flag) { m_is_inline = flag; }

  void add_output_operand (const char *asm_symbolic_name,
			   const char *constraint,
			   lvalue *dest);
  void add_input_operand (const char *asm_symbolic_name,
			  const char *constraint,
			  rvalue *src);
  void add_clobber (const char *victim);

  void replay_into (replayer *r) override;

  string *get_asm_template () const { return m_asm_template; }

  virtual bool is_goto () const = 0;
  virtual void maybe_print_gotos (pretty_printer *) const = 0;

protected:
  void write_flags (reproducer &r);
  void write_clobbers (reproducer &r);

private:
  string * make_debug_string () final override;
  virtual void maybe_populate_playback_blocks
    (auto_vec <playback::block *> *out) = 0;

protected:
  string *m_asm_template;
  bool m_is_volatile;
  bool m_is_inline;
  auto_vec<output_asm_operand *> m_output_ops;
  auto_vec<input_asm_operand *> m_input_ops;
  auto_vec<string *> m_clobbers;
};

/* An extended_asm that's not a goto, as created by
   gcc_jit_block_add_extended_asm. */

class extended_asm_simple : public extended_asm
{
public:
  extended_asm_simple (block *b,
		       location *loc,
		       string *asm_template)
  : extended_asm (b, loc, asm_template)
  {}

  void write_reproducer (reproducer &r) override;
  bool is_goto () const final override { return false; }
  void maybe_print_gotos (pretty_printer *) const final override {}

private:
  void maybe_populate_playback_blocks
    (auto_vec <playback::block *> *) final override
  {}
};

/* An extended_asm that's a asm goto, as created by
   gcc_jit_block_end_with_extended_asm_goto.  */

class extended_asm_goto : public extended_asm
{
public:
  extended_asm_goto (block *b,
		     location *loc,
		     string *asm_template,
		     int num_goto_blocks,
		     block **goto_blocks,
		     block *fallthrough_block);

  void replay_into (replayer *r) final override;
  void write_reproducer (reproducer &r) override;

  vec <block *> get_successor_blocks () const final override;

  bool is_goto () const final override { return true; }
  void maybe_print_gotos (pretty_printer *) const final override;

private:
  void maybe_populate_playback_blocks
    (auto_vec <playback::block *> *out) final override;

private:
  auto_vec <block *> m_goto_blocks;
  block *m_fallthrough_block;
};

/* A group of top-level asm statements, as created by
   gcc_jit_context_add_top_level_asm.  */

class top_level_asm : public memento
{
public:
  top_level_asm (context *ctxt, location *loc, string *asm_stmts);

  void write_to_dump (dump &d) final override;

private:
  void replay_into (replayer *r) final override;
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  location *m_loc;
  string *m_asm_stmts;
};

class global_init_rvalue : public memento
{
public:
  global_init_rvalue (context *ctxt, lvalue *variable, rvalue *init) :
    memento (ctxt), m_variable (variable), m_init (init) {};

  void write_to_dump (dump &d) final override;

private:
  void replay_into (replayer *r) final override;
  string * make_debug_string () final override;
  void write_reproducer (reproducer &r) final override;

private:
  lvalue *m_variable;
  rvalue *m_init;
};

} // namespace gcc::jit::recording

/* Create a recording::memento_of_new_rvalue_from_const instance and add
   it to this context's list of mementos.

   Implements the post-error-checking part of
   gcc_jit_context_new_rvalue_from_{int|long|double|ptr}.  */

template <typename HOST_TYPE>
recording::rvalue *
recording::context::new_rvalue_from_const (recording::type *type,
					   HOST_TYPE value)
{
  recording::rvalue *result =
    new memento_of_new_rvalue_from_const <HOST_TYPE> (this, NULL, type, value);
  record (result);
  return result;
}

/* Don't call this directly.  Call types_kinda_same.  */
bool
types_kinda_same_internal (recording::type *a,
			   recording::type *b);

/* Strip all qualifiers and count pointer depth, returning true
   if the types and pointer depth are the same, otherwise false.

   For array and vector types the number of element also
   has to match, aswell as the element types themself.  */
inline bool
types_kinda_same (recording::type *a, recording::type *b)
{
  /* Handle trivial case here, to allow for inlining.  */
  return a == b || types_kinda_same_internal (a, b);
}

} // namespace gcc::jit

} // namespace gcc

#endif /* JIT_RECORDING_H */
