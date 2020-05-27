/* Internals of libgccjit: classes for playing back recorded API calls.
   Copyright (C) 2013-2020 Free Software Foundation, Inc.
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

#ifndef JIT_PLAYBACK_H
#define JIT_PLAYBACK_H

#include <utility> // for std::pair

#include "timevar.h"

#include "jit-recording.h"

struct diagnostic_context;
struct diagnostic_info;

namespace gcc {

namespace jit {

/**********************************************************************
 Playback.
 **********************************************************************/

namespace playback {

/* playback::context is an abstract base class.

   The two concrete subclasses are:
   - playback::compile_to_memory
   - playback::compile_to_file.  */

class context : public log_user
{
public:
  context (::gcc::jit::recording::context *ctxt);
  ~context ();

  void gt_ggc_mx ();

  void replay ();

  location *
  new_location (recording::location *rloc,
		const char *filename,
		int line,
		int column);

  type *
  get_type (enum gcc_jit_types type);

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

  compound_type *
  new_compound_type (location *loc,
		     const char *name,
		     bool is_struct); /* else is union */

  type *
  new_function_type (type *return_type,
		     const auto_vec<type *> *param_types,
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
		const auto_vec<param *> *params,
		int is_variadic,
		enum built_in_function builtin_id);

  lvalue *
  new_global (location *loc,
	      enum gcc_jit_global_kind kind,
	      type *type,
	      const char *name);

  template <typename HOST_TYPE>
  rvalue *
  new_rvalue_from_const (type *type,
			 HOST_TYPE value);

  rvalue *
  new_string_literal (const char *value);

  rvalue *
  new_rvalue_from_vector (location *loc,
			  type *type,
			  const auto_vec<rvalue *> &elements);

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
	    const auto_vec<rvalue *> *args,
	    bool require_tail_call);

  rvalue *
  new_call_through_ptr (location *loc,
			rvalue *fn_ptr,
			const auto_vec<rvalue *> *args,
			bool require_tail_call);

  rvalue *
  new_cast (location *loc,
	    rvalue *expr,
	    type *type_);

  lvalue *
  new_array_access (location *loc,
		    rvalue *ptr,
		    rvalue *index);

  void
  set_str_option (enum gcc_jit_str_option opt,
		  const char *value);

  void
  set_int_option (enum gcc_jit_int_option opt,
		  int value);

  void
  set_bool_option (enum gcc_jit_bool_option opt,
		   int value);

  const char *
  get_str_option (enum gcc_jit_str_option opt) const
  {
    return m_recording_ctxt->get_str_option (opt);
  }

  int
  get_int_option (enum gcc_jit_int_option opt) const
  {
    return m_recording_ctxt->get_int_option (opt);
  }

  int
  get_bool_option (enum gcc_jit_bool_option opt) const
  {
    return m_recording_ctxt->get_bool_option (opt);
  }

  int
  get_inner_bool_option (enum inner_bool_option opt) const
  {
    return m_recording_ctxt->get_inner_bool_option (opt);
  }

  builtins_manager *get_builtins_manager () const
  {
    return m_recording_ctxt->get_builtins_manager ();
  }

  void
  compile ();

  void
  add_error (location *loc, const char *fmt, ...)
      GNU_PRINTF(3, 4);

  void
  add_error_va (location *loc, const char *fmt, va_list ap)
      GNU_PRINTF(3, 0);

  const char *
  get_first_error () const;

  void
  add_diagnostic (struct diagnostic_context *context,
		  struct diagnostic_info *diagnostic);

  void
  set_tree_location (tree t, location *loc);

  tree
  new_field_access (location *loc,
		    tree datum,
		    field *field);

  tree
  new_dereference (tree ptr, location *loc);

  tree
  as_truth_value (tree expr, location *loc);

  bool errors_occurred () const
  {
    return m_recording_ctxt->errors_occurred ();
  }

  timer *get_timer () const { return m_recording_ctxt->get_timer (); }

private:
  void dump_generated_code ();

  rvalue *
  build_call (location *loc,
	      tree fn_ptr,
	      const auto_vec<rvalue *> *args,
	      bool require_tail_call);

  tree
  build_cast (location *loc,
	      rvalue *expr,
	      type *type_);

  source_file *
  get_source_file (const char *filename);

  void handle_locations ();

  const char * get_path_c_file () const;
  const char * get_path_s_file () const;
  const char * get_path_so_file () const;

private:

  /* Functions for implementing "compile".  */

  void acquire_mutex ();
  void release_mutex ();

  void
  make_fake_args (vec <char *> *argvec,
		  const char *ctxt_progname,
		  vec <recording::requested_dump> *requested_dumps);

  void
  extract_any_requested_dumps
    (vec <recording::requested_dump> *requested_dumps);

  char *
  read_dump_file (const char *path);

  virtual void postprocess (const char *ctxt_progname) = 0;

protected:
  tempdir *get_tempdir () { return m_tempdir; }

  void
  convert_to_dso (const char *ctxt_progname);

  void
  invoke_driver (const char *ctxt_progname,
		 const char *input_file,
		 const char *output_file,
		 timevar_id_t tv_id,
		 bool shared,
		 bool run_linker);

  void
  add_multilib_driver_arguments (vec <char *> *argvec);

  result *
  dlopen_built_dso ();

 private:
  void
  invoke_embedded_driver (const vec <char *> *argvec);

  void
  invoke_external_driver (const char *ctxt_progname,
			  vec <char *> *argvec);

private:
  ::gcc::jit::recording::context *m_recording_ctxt;

  tempdir *m_tempdir;

  auto_vec<function *> m_functions;
  auto_vec<tree> m_globals;
  tree m_const_char_ptr;

  /* Source location handling.  */
  auto_vec<source_file *> m_source_files;

  auto_vec<std::pair<tree, location *> > m_cached_locations;
};

class compile_to_memory : public context
{
 public:
  compile_to_memory (recording::context *ctxt);
  void postprocess (const char *ctxt_progname) FINAL OVERRIDE;

  result *get_result_obj () const { return m_result; }

 private:
  result *m_result;
};

class compile_to_file : public context
{
 public:
  compile_to_file (recording::context *ctxt,
		   enum gcc_jit_output_kind output_kind,
		   const char *output_path);
  void postprocess (const char *ctxt_progname) FINAL OVERRIDE;

 private:
  void
  copy_file (const char *src_path,
	     const char *dst_path);

 private:
  enum gcc_jit_output_kind m_output_kind;
  const char *m_output_path;
};


/* A temporary wrapper object.
   These objects are (mostly) only valid during replay.
   We allocate them on the GC heap, so that they will be cleaned
   the next time the GC collects.
   The exception is the "function" class, which is tracked and marked by
   the jit::context, since it needs to stay alive during post-processing
   (when the GC could run). */
class wrapper
{
public:
  /* Allocate in the GC heap.  */
  void *operator new (size_t sz);

  /* Some wrapper subclasses contain vec<> and so need to
     release them when they are GC-ed.  */
  virtual void finalizer () { }

};

class type : public wrapper
{
public:
  type (tree inner)
    : m_inner(inner)
  {}

  tree as_tree () const { return m_inner; }

  type *get_pointer () const { return new type (build_pointer_type (m_inner)); }

  type *get_const () const
  {
    return new type (build_qualified_type (m_inner, TYPE_QUAL_CONST));
  }

  type *get_volatile () const
  {
    return new type (build_qualified_type (m_inner, TYPE_QUAL_VOLATILE));
  }

  type *get_aligned (size_t alignment_in_bytes) const;
  type *get_vector (size_t num_units) const;

private:
  tree m_inner;
};

class compound_type : public type
{
public:
  compound_type (tree inner)
    : type (inner)
  {}

  void set_fields (const auto_vec<field *> *fields);
};

class field : public wrapper
{
public:
  field (tree inner)
    : m_inner(inner)
  {}

  tree as_tree () const { return m_inner; }

private:
  tree m_inner;
};

class bitfield : public field {};

class function : public wrapper
{
public:
  function(context *ctxt, tree fndecl, enum gcc_jit_function_kind kind);

  void gt_ggc_mx ();
  void finalizer () FINAL OVERRIDE;

  tree get_return_type_as_tree () const;

  tree as_fndecl () const { return m_inner_fndecl; }

  enum gcc_jit_function_kind get_kind () const { return m_kind; }

  lvalue *
  new_local (location *loc,
	     type *type,
	     const char *name);

  block*
  new_block (const char *name);

  rvalue *
  get_address (location *loc);

  void
  build_stmt_list ();

  void
  postprocess ();

public:
  context *m_ctxt;

public:
  void
  set_tree_location (tree t, location *loc)
  {
    m_ctxt->set_tree_location (t, loc);
  }

private:
  tree m_inner_fndecl;
  tree m_inner_block;
  tree m_inner_bind_expr;
  enum gcc_jit_function_kind m_kind;
  tree m_stmt_list;
  tree_stmt_iterator m_stmt_iter;
  vec<block *> m_blocks;
};

struct case_
{
  case_ (rvalue *min_value, rvalue *max_value, block *dest_block)
  : m_min_value (min_value),
    m_max_value (max_value),
    m_dest_block (dest_block)
  {}

  rvalue *m_min_value;
  rvalue *m_max_value;
  block *m_dest_block;
};

class block : public wrapper
{
public:
  block (function *func,
	 const char *name);

  void finalizer () FINAL OVERRIDE;

  tree as_label_decl () const { return m_label_decl; }

  function *get_function () const { return m_func; }

  void
  add_eval (location *loc,
	    rvalue *rvalue);

  void
  add_assignment (location *loc,
		  lvalue *lvalue,
		  rvalue *rvalue);

  void
  add_comment (location *loc,
	       const char *text);

  void
  add_conditional (location *loc,
		   rvalue *boolval,
		   block *on_true,
		   block *on_false);

  block *
  add_block (location *loc,
	     const char *name);

  void
  add_jump (location *loc,
	    block *target);

  void
  add_return (location *loc,
	      rvalue *rvalue);

  void
  add_switch (location *loc,
	      rvalue *expr,
	      block *default_block,
	      const auto_vec <case_> *cases);

private:
  void
  set_tree_location (tree t, location *loc)
  {
    m_func->set_tree_location (t, loc);
  }

  void add_stmt (tree stmt)
  {
    /* TODO: use one stmt_list per block.  */
    m_stmts.safe_push (stmt);
  }

private:
  function *m_func;
  tree m_label_decl;
  vec<tree> m_stmts;

public: // for now
  tree m_label_expr;

  friend class function;
};

class rvalue : public wrapper
{
public:
  rvalue (context *ctxt, tree inner)
    : m_ctxt (ctxt),
      m_inner (inner)
  {
    /* Pre-mark tree nodes with TREE_VISITED so that they can be
       deeply unshared during gimplification (including across
       functions); this requires LANG_HOOKS_DEEP_UNSHARING to be true.  */
    TREE_VISITED (inner) = 1;
  }

  rvalue *
  as_rvalue () { return this; }

  tree as_tree () const { return m_inner; }

  context *get_context () const { return m_ctxt; }

  type *
  get_type () { return new type (TREE_TYPE (m_inner)); }

  rvalue *
  access_field (location *loc,
		field *field);

  lvalue *
  dereference_field (location *loc,
		     field *field);

  lvalue *
  dereference (location *loc);

private:
  context *m_ctxt;
  tree m_inner;
};

class lvalue : public rvalue
{
public:
  lvalue (context *ctxt, tree inner)
    : rvalue(ctxt, inner)
  {}

  lvalue *
  as_lvalue () { return this; }

  lvalue *
  access_field (location *loc,
		field *field);

  rvalue *
  get_address (location *loc);

private:
  bool mark_addressable (location *loc);
};

class param : public lvalue
{
public:
  param (context *ctxt, tree inner)
    : lvalue(ctxt, inner)
  {}
};

/* Dealing with the linemap API.

   It appears that libcpp requires locations to be created as if by
   a tokenizer, creating them by filename, in ascending order of
   line/column, whereas our API doesn't impose any such constraints:
   we allow client code to create locations in arbitrary orders.

   To square this circle, we need to cache all location creation,
   grouping things up by filename/line, and then creating the linemap
   entries in a post-processing phase.  */

/* A set of locations, all sharing a filename */
class source_file : public wrapper
{
public:
  source_file (tree filename);
  void finalizer () FINAL OVERRIDE;

  source_line *
  get_source_line (int line_num);

  tree filename_as_tree () const { return m_filename; }

  const char*
  get_filename () const { return IDENTIFIER_POINTER (m_filename); }

  vec<source_line *> m_source_lines;

private:
  tree m_filename;
};

/* A source line, with one or more locations of interest.  */
class source_line : public wrapper
{
public:
  source_line (source_file *file, int line_num);
  void finalizer () FINAL OVERRIDE;

  location *
  get_location (recording::location *rloc, int column_num);

  int get_line_num () const { return m_line_num; }

  vec<location *> m_locations;

private:
  source_file *m_source_file;
  int m_line_num;
};

/* A specific location on a source line.  This is what we expose
   to the client API.  */
class location : public wrapper
{
public:
  location (recording::location *loc, source_line *line, int column_num);

  int get_column_num () const { return m_column_num; }

  recording::location *get_recording_loc () const { return m_recording_loc; }

  location_t m_srcloc;

private:
  recording::location *m_recording_loc;
  source_line *m_line;
  int m_column_num;
};

} // namespace gcc::jit::playback

extern playback::context *active_playback_ctxt;

} // namespace gcc::jit

} // namespace gcc

#endif /* JIT_PLAYBACK_H */
