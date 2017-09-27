/* A C++ API for libgccjit, purely as inline wrapper functions.
   Copyright (C) 2014-2017 Free Software Foundation, Inc.

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

#ifndef LIBGCCJIT_PLUS_PLUS_H
#define LIBGCCJIT_PLUS_PLUS_H

#include "libgccjit.h"

#include <limits>
#include <ostream>
#include <vector>

/****************************************************************************
 C++ API
 ****************************************************************************/

namespace gccjit
{
  /* Indentation indicates inheritance.  */
  class context;
  class error;
  class object;
    class location;
    class field;
    class type;
      class struct_;
    class function;
    class block;
    class rvalue;
     class lvalue;
       class param;
    class case_;
  class timer;
  class auto_time;

  /* Errors within the API become C++ exceptions of this class.  */
  class error
  {
  };

  class object
  {
  public:
    context get_context () const;

    std::string get_debug_string () const;

  protected:
    object ();
    object (gcc_jit_object *obj);

    gcc_jit_object *get_inner_object () const;

  private:
    gcc_jit_object *m_inner_obj;
  };

  inline std::ostream& operator << (std::ostream& stream, const object &obj);

  /* Some client code will want to supply source code locations, others
     won't.  To avoid doubling the number of entrypoints, everything
     accepting a location also has a default argument.  To do this, the
     other classes need to see that "location" has a default constructor,
     hence we need to declare it first.  */
  class location : public object
  {
  public:
    location ();
    location (gcc_jit_location *loc);

    gcc_jit_location *get_inner_location () const;
  };

  class context
  {
  public:
    static context acquire ();
    context ();
    context (gcc_jit_context *ctxt);

    gccjit::context new_child_context ();

    gcc_jit_context *get_inner_context () { return m_inner_ctxt; }

    void release ();

    gcc_jit_result *compile ();

    void compile_to_file (enum gcc_jit_output_kind output_kind,
			  const char *output_path);

    void dump_to_file (const std::string &path,
		       bool update_locations);

    void set_logfile (FILE *logfile,
		      int flags,
		      int verbosity);

    void dump_reproducer_to_file (const char *path);

    void set_str_option (enum gcc_jit_str_option opt,
			 const char *value);

    void set_int_option (enum gcc_jit_int_option opt,
			 int value);

    void set_bool_option (enum gcc_jit_bool_option opt,
			  int value);

    void set_bool_allow_unreachable_blocks (int bool_value);
    void set_bool_use_external_driver (int bool_value);

    void add_command_line_option (const char *optname);

    void set_timer (gccjit::timer t);
    gccjit::timer get_timer () const;

    location
    new_location (const std::string &filename,
		  int line,
		  int column);

    type get_type (enum gcc_jit_types kind);
    type get_int_type (size_t num_bytes, int is_signed);

    /* A way to map a specific int type, using the compiler to
       get the details automatically e.g.:
	  gccjit::type type = get_int_type <my_int_type_t> ();  */
    template <typename T>
    type get_int_type ();

    type new_array_type (type element_type, int num_elements,
			 location loc = location ());

    field new_field (type type_, const std::string &name,
		     location loc = location ());

    struct_ new_struct_type (const std::string &name,
			     std::vector<field> &fields,
			     location loc = location ());

    struct_ new_opaque_struct_type (const std::string &name,
				    location loc = location ());

    param new_param (type type_,
		     const std::string &name,
		     location loc = location ());

    function new_function (enum gcc_jit_function_kind kind,
			   type return_type,
			   const std::string &name,
			   std::vector<param> &params,
			   int is_variadic,
			   location loc = location ());

    function get_builtin_function (const std::string &name);

    lvalue new_global (enum gcc_jit_global_kind kind,
		       type type_,
		       const std::string &name,
		       location loc = location ());

    rvalue new_rvalue (type numeric_type,
		       int value) const;
    rvalue new_rvalue (type numeric_type,
		       long value) const;
    rvalue zero (type numeric_type) const;
    rvalue one (type numeric_type) const;
    rvalue new_rvalue (type numeric_type,
		       double value) const;
    rvalue new_rvalue (type pointer_type,
		       void *value) const;
    rvalue new_rvalue (const std::string &value) const;

    /* Generic unary operations...  */
    rvalue new_unary_op (enum gcc_jit_unary_op op,
			 type result_type,
			 rvalue a,
			 location loc = location ());

    /* ...and shorter ways to spell the various specific kinds of
       unary op.  */
    rvalue new_minus (type result_type,
		      rvalue a,
		      location loc = location ());
    rvalue new_bitwise_negate (type result_type,
			       rvalue a,
			       location loc = location ());
    rvalue new_logical_negate (type result_type,
			       rvalue a,
			       location loc = location ());

    /* Generic binary operations...  */
    rvalue new_binary_op (enum gcc_jit_binary_op op,
			  type result_type,
			  rvalue a, rvalue b,
			  location loc = location ());

    /* ...and shorter ways to spell the various specific kinds of
       binary op.  */
    rvalue new_plus (type result_type,
		     rvalue a, rvalue b,
		     location loc = location ());
    rvalue new_minus (type result_type,
		      rvalue a, rvalue b,
		      location loc = location ());
    rvalue new_mult (type result_type,
		     rvalue a, rvalue b,
		     location loc = location ());
    rvalue new_divide (type result_type,
		       rvalue a, rvalue b,
		       location loc = location ());
    rvalue new_modulo (type result_type,
		       rvalue a, rvalue b,
		       location loc = location ());
    rvalue new_bitwise_and (type result_type,
			    rvalue a, rvalue b,
			    location loc = location ());
    rvalue new_bitwise_xor (type result_type,
			    rvalue a, rvalue b,
			    location loc = location ());
    rvalue new_bitwise_or (type result_type,
			   rvalue a, rvalue b,
			   location loc = location ());
    rvalue new_logical_and (type result_type,
			    rvalue a, rvalue b,
			    location loc = location ());
    rvalue new_logical_or (type result_type,
			   rvalue a, rvalue b,
			   location loc = location ());

    /* Generic comparisons...  */
    rvalue new_comparison (enum gcc_jit_comparison op,
			   rvalue a, rvalue b,
			   location loc = location ());
    /* ...and shorter ways to spell the various specific kinds of
       comparison.  */
    rvalue new_eq (rvalue a, rvalue b,
		   location loc = location ());
    rvalue new_ne (rvalue a, rvalue b,
		   location loc = location ());
    rvalue new_lt (rvalue a, rvalue b,
		   location loc = location ());
    rvalue new_le (rvalue a, rvalue b,
		   location loc = location ());
    rvalue new_gt (rvalue a, rvalue b,
		   location loc = location ());
    rvalue new_ge (rvalue a, rvalue b,
		   location loc = location ());

    /* The most general way of creating a function call.  */
    rvalue new_call (function func,
		     std::vector<rvalue> &args,
		     location loc = location ());

    /* In addition, we provide a series of overloaded "new_call" methods
       for specific numbers of args (from 0 - 6), to avoid the need for
       client code to manually build a vector.  */
    rvalue new_call (function func,
		     location loc = location ());
    rvalue new_call (function func,
		     rvalue arg0,
		     location loc = location ());
    rvalue new_call (function func,
		     rvalue arg0, rvalue arg1,
		     location loc = location ());
    rvalue new_call (function func,
		     rvalue arg0, rvalue arg1, rvalue arg2,
		     location loc = location ());
    rvalue new_call (function func,
		     rvalue arg0, rvalue arg1, rvalue arg2,
		     rvalue arg3,
		     location loc = location ());
    rvalue new_call (function func,
		     rvalue arg0, rvalue arg1, rvalue arg2,
		     rvalue arg3, rvalue arg4,
		     location loc = location ());
    rvalue new_call (function func,
		     rvalue arg0, rvalue arg1, rvalue arg2,
		     rvalue arg3, rvalue arg4, rvalue arg5,
		     location loc = location ());

    rvalue new_cast (rvalue expr,
		     type type_,
		     location loc = location ());

    lvalue new_array_access (rvalue ptr,
			     rvalue index,
			     location loc = location ());

    case_ new_case (rvalue min_value,
		    rvalue max_value,
		    block dest_block);

  private:
    gcc_jit_context *m_inner_ctxt;
  };

  class field : public object
  {
  public:
    field ();
    field (gcc_jit_field *inner);

    gcc_jit_field *get_inner_field () const;
  };

  class type : public object
  {
  public:
    type ();
    type (gcc_jit_type *inner);

    gcc_jit_type *get_inner_type () const;

    type get_pointer ();
    type get_const ();
    type get_volatile ();
    type get_aligned (size_t alignment_in_bytes);
    type get_vector (size_t num_units);

    // Shortcuts for getting values of numeric types:
    rvalue zero ();
    rvalue one ();
 };

  class struct_ : public type
  {
  public:
    struct_ ();
    struct_ (gcc_jit_struct *inner);

    gcc_jit_struct *get_inner_struct () const;
  };

  class function : public object
  {
  public:
    function ();
    function (gcc_jit_function *func);

    gcc_jit_function *get_inner_function () const;

    void dump_to_dot (const std::string &path);

    param get_param (int index) const;

    block new_block ();
    block new_block (const std::string &name);

    lvalue new_local (type type_,
		      const std::string &name,
		      location loc = location ());

    rvalue get_address (location loc = location ());

    /* A series of overloaded operator () with various numbers of arguments
       for a very terse way of creating a call to this function.  The call
       is created within the same context as the function itself, which may
       not be what you want.  */
    rvalue operator() (location loc = location ());
    rvalue operator() (rvalue arg0,
		       location loc = location ());
    rvalue operator() (rvalue arg0, rvalue arg1,
		       location loc = location ());
    rvalue operator() (rvalue arg0, rvalue arg1, rvalue arg2,
		       location loc = location ());
  };

  class block : public object
  {
  public:
    block ();
    block (gcc_jit_block *inner);

    gcc_jit_block *get_inner_block () const;

    function get_function () const;

    void add_eval (rvalue rvalue,
		   location loc = location ());

    void add_assignment (lvalue lvalue,
			 rvalue rvalue,
			 location loc = location ());

    void add_assignment_op (lvalue lvalue,
			    enum gcc_jit_binary_op op,
			    rvalue rvalue,
			    location loc = location ());

    /* A way to add a function call to the body of a function being
       defined, with various numbers of args.  */
    rvalue add_call (function other,
		     location loc = location ());
    rvalue add_call (function other,
		     rvalue arg0,
		     location loc = location ());
    rvalue add_call (function other,
		     rvalue arg0, rvalue arg1,
		     location loc = location ());
    rvalue add_call (function other,
		     rvalue arg0, rvalue arg1, rvalue arg2,
		     location loc = location ());
    rvalue add_call (function other,
		     rvalue arg0, rvalue arg1, rvalue arg2, rvalue arg3,
		     location loc = location ());

    void add_comment (const std::string &text,
		      location loc = location ());

    void end_with_conditional (rvalue boolval,
			       block on_true,
			       block on_false,
			       location loc = location ());

    void end_with_jump (block target,
			location loc = location ());

    void end_with_return (rvalue rvalue,
			  location loc = location ());
    void end_with_return (location loc = location ());

    void end_with_switch (rvalue expr,
			  block default_block,
			  std::vector <case_> cases,
			  location loc = location ());
  };

  class rvalue : public object
  {
  public:
    rvalue ();
    rvalue (gcc_jit_rvalue *inner);
    gcc_jit_rvalue *get_inner_rvalue () const;

    type get_type ();

    rvalue access_field (field field,
			 location loc = location ());

    lvalue dereference_field (field field,
			      location loc = location ());

    lvalue dereference (location loc = location ());

    rvalue cast_to (type type_,
		    location loc = location ());

    /* Array access.  */
    lvalue operator[] (rvalue index);
    lvalue operator[] (int index);
  };

  class lvalue : public rvalue
  {
  public:
    lvalue ();
    lvalue (gcc_jit_lvalue *inner);

    gcc_jit_lvalue *get_inner_lvalue () const;

    lvalue access_field (field field,
			 location loc = location ());

    rvalue get_address (location loc = location ());
  };

  class param : public lvalue
  {
  public:
    param ();
    param (gcc_jit_param *inner);

    gcc_jit_param *get_inner_param () const;
  };

  class case_ : public object
  {
  public:
    case_ ();
    case_ (gcc_jit_case *inner);

    gcc_jit_case *get_inner_case () const;
  };

  /* Overloaded operators, for those who want the most terse API
     (at the possible risk of being a little too magical).

     In each case, the first parameter is used to determine which context
     owns the resulting expression, and, where appropriate,  what the
     latter's type is. */

  /* Unary operators.  */
  rvalue operator- (rvalue a); // unary minus
  rvalue operator~ (rvalue a); // unary bitwise negate
  rvalue operator! (rvalue a); // unary logical negate

  /* Binary operators.  */
  rvalue operator+ (rvalue a, rvalue b);
  rvalue operator- (rvalue a, rvalue b);
  rvalue operator* (rvalue a, rvalue b);
  rvalue operator/ (rvalue a, rvalue b);
  rvalue operator% (rvalue a, rvalue b);
  rvalue operator& (rvalue a, rvalue b); //  bitwise and
  rvalue operator^ (rvalue a, rvalue b); // bitwise_xor
  rvalue operator| (rvalue a, rvalue b); // bitwise_or
  rvalue operator&& (rvalue a, rvalue b); // logical_and
  rvalue operator|| (rvalue a, rvalue b); // logical_or

  /* Comparisons.  */
  rvalue operator== (rvalue a, rvalue b);
  rvalue operator!= (rvalue a, rvalue b);
  rvalue operator< (rvalue a, rvalue b);
  rvalue operator<= (rvalue a, rvalue b);
  rvalue operator> (rvalue a, rvalue b);
  rvalue operator>= (rvalue a, rvalue b);

  /* Dereferencing. */
  lvalue operator* (rvalue ptr);

  class timer
  {
  public:
    timer ();
    timer (gcc_jit_timer *inner_timer);

    void push (const char *item_name);
    void pop (const char *item_name);
    void print (FILE *f_out) const;

    void release ();

    gcc_jit_timer *get_inner_timer () const;

  private:
    gcc_jit_timer *m_inner_timer;
  };

  class auto_time
  {
  public:
    auto_time (timer t, const char *item_name);
    auto_time (context ctxt, const char *item_name);
    ~auto_time ();

  private:
    timer m_timer;
    const char *m_item_name;
  };
}

/****************************************************************************
 Implementation of the API
 ****************************************************************************/
namespace gccjit {

// class context
inline context context::acquire ()
{
  return context (gcc_jit_context_acquire ());
}
inline context::context () : m_inner_ctxt (NULL) {}
inline context::context (gcc_jit_context *inner) : m_inner_ctxt (inner)
{
  if (!inner)
    throw error ();
}

inline gccjit::context
context::new_child_context ()
{
  return context (gcc_jit_context_new_child_context (m_inner_ctxt));
}

inline void
context::release ()
{
  gcc_jit_context_release (m_inner_ctxt);
  m_inner_ctxt = NULL;
}

inline gcc_jit_result *
context::compile ()
{
  gcc_jit_result *result = gcc_jit_context_compile (m_inner_ctxt);
  if (!result)
    throw error ();
  return result;
}

inline void
context::compile_to_file (enum gcc_jit_output_kind output_kind,
			  const char *output_path)
{
  gcc_jit_context_compile_to_file (m_inner_ctxt,
				   output_kind,
				   output_path);
}

inline void
context::dump_to_file (const std::string &path,
		       bool update_locations)
{
  gcc_jit_context_dump_to_file (m_inner_ctxt,
				path.c_str (),
				update_locations);
}

inline void
context::set_logfile (FILE *logfile,
		      int flags,
		      int verbosity)
{
  gcc_jit_context_set_logfile (m_inner_ctxt,
			       logfile,
			       flags,
			       verbosity);
}

inline void
context::dump_reproducer_to_file (const char *path)
{
  gcc_jit_context_dump_reproducer_to_file (m_inner_ctxt,
					   path);
}

inline void
context::set_str_option (enum gcc_jit_str_option opt,
			 const char *value)
{
  gcc_jit_context_set_str_option (m_inner_ctxt, opt, value);

}

inline void
context::set_int_option (enum gcc_jit_int_option opt,
			 int value)
{
  gcc_jit_context_set_int_option (m_inner_ctxt, opt, value);

}

inline void
context::set_bool_option (enum gcc_jit_bool_option opt,
			  int value)
{
  gcc_jit_context_set_bool_option (m_inner_ctxt, opt, value);
}

inline void
context::set_bool_allow_unreachable_blocks (int bool_value)
{
  gcc_jit_context_set_bool_allow_unreachable_blocks (m_inner_ctxt,
						     bool_value);
}

inline void
context::set_bool_use_external_driver (int bool_value)
{
  gcc_jit_context_set_bool_use_external_driver (m_inner_ctxt,
						bool_value);
}

inline void
context::add_command_line_option (const char *optname)
{
  gcc_jit_context_add_command_line_option (m_inner_ctxt, optname);
}

inline void
context::set_timer (gccjit::timer t)
{
  gcc_jit_context_set_timer (m_inner_ctxt, t.get_inner_timer ());
}

inline gccjit::timer
context::get_timer () const
{
  return gccjit::timer (gcc_jit_context_get_timer (m_inner_ctxt));
}


inline location
context::new_location (const std::string &filename,
		       int line,
		       int column)
{
  return location (gcc_jit_context_new_location (m_inner_ctxt,
						 filename.c_str (),
						 line,
						 column));
}

inline type
context::get_type (enum gcc_jit_types kind)
{
  return type (gcc_jit_context_get_type (m_inner_ctxt, kind));
}

inline type
context::get_int_type (size_t num_bytes, int is_signed)
{
  return type (gcc_jit_context_get_int_type (m_inner_ctxt,
					     num_bytes,
					     is_signed));
}

template <typename T>
inline type
context::get_int_type ()
{
  return get_int_type (sizeof (T), std::numeric_limits<T>::is_signed);
}

inline type
context::new_array_type (type element_type, int num_elements, location loc)
{
  return type (gcc_jit_context_new_array_type (
		 m_inner_ctxt,
		 loc.get_inner_location (),
		 element_type.get_inner_type (),
		 num_elements));
}

inline field
context::new_field (type type_, const std::string &name, location loc)
{
  return field (gcc_jit_context_new_field (m_inner_ctxt,
					   loc.get_inner_location (),
					   type_.get_inner_type (),
					   name.c_str ()));
}

inline struct_
context::new_struct_type (const std::string &name,
			  std::vector<field> &fields,
			  location loc)
{
  /* Treat std::vector as an array, relying on it not being resized: */
  field *as_array_of_wrappers = &fields[0];

  /* Treat the array as being of the underlying pointers, relying on
     the wrapper type being such a pointer internally.	*/
  gcc_jit_field **as_array_of_ptrs =
    reinterpret_cast<gcc_jit_field **> (as_array_of_wrappers);

  return struct_ (gcc_jit_context_new_struct_type (m_inner_ctxt,
						   loc.get_inner_location (),
						   name.c_str (),
						   fields.size (),
						   as_array_of_ptrs));
}

inline struct_
context::new_opaque_struct_type (const std::string &name,
				 location loc)
{
  return struct_ (gcc_jit_context_new_opaque_struct (
		    m_inner_ctxt,
		    loc.get_inner_location (),
		    name.c_str ()));
}

inline param
context::new_param (type type_,
		    const std::string &name,
		    location loc)
{
  return param (gcc_jit_context_new_param (m_inner_ctxt,
					   loc.get_inner_location (),
					   type_.get_inner_type (),
					   name.c_str ()));
}

inline function
context::new_function (enum gcc_jit_function_kind kind,
		       type return_type,
		       const std::string &name,
		       std::vector<param> &params,
		       int is_variadic,
		       location loc)
{
  /* Treat std::vector as an array, relying on it not being resized: */
  param *as_array_of_wrappers = &params[0];

  /* Treat the array as being of the underlying pointers, relying on
     the wrapper type being such a pointer internally.	*/
  gcc_jit_param **as_array_of_ptrs =
    reinterpret_cast<gcc_jit_param **> (as_array_of_wrappers);

  return function (gcc_jit_context_new_function (m_inner_ctxt,
						 loc.get_inner_location (),
						 kind,
						 return_type.get_inner_type (),
						 name.c_str (),
						 params.size (),
						 as_array_of_ptrs,
						 is_variadic));
}

inline function
context::get_builtin_function (const std::string &name)
{
  return function (gcc_jit_context_get_builtin_function (m_inner_ctxt,
							 name.c_str ()));
}

inline lvalue
context::new_global (enum gcc_jit_global_kind kind,
		     type type_,
		     const std::string &name,
		     location loc)
{
  return lvalue (gcc_jit_context_new_global (m_inner_ctxt,
					     loc.get_inner_location (),
					     kind,
					     type_.get_inner_type (),
					     name.c_str ()));
}

inline rvalue
context::new_rvalue (type numeric_type,
		     int value) const
{
  return rvalue (
    gcc_jit_context_new_rvalue_from_int (m_inner_ctxt,
					 numeric_type.get_inner_type (),
					 value));
}

inline rvalue
context::new_rvalue (type numeric_type,
		     long value) const
{
  return rvalue (
    gcc_jit_context_new_rvalue_from_long (m_inner_ctxt,
					  numeric_type.get_inner_type (),
					  value));
}

inline rvalue
context::zero (type numeric_type) const
{
  return rvalue (gcc_jit_context_zero (m_inner_ctxt,
				       numeric_type.get_inner_type ()));
}

inline rvalue
context::one (type numeric_type) const
{
  return rvalue (gcc_jit_context_one (m_inner_ctxt,
				       numeric_type.get_inner_type ()));
}

inline rvalue
context::new_rvalue (type numeric_type,
		     double value) const
{
  return rvalue (
    gcc_jit_context_new_rvalue_from_double (m_inner_ctxt,
					    numeric_type.get_inner_type (),
					    value));
}

inline rvalue
context::new_rvalue (type pointer_type,
		     void *value) const
{
  return rvalue (
    gcc_jit_context_new_rvalue_from_ptr (m_inner_ctxt,
					 pointer_type.get_inner_type (),
					 value));
}

inline rvalue
context::new_rvalue (const std::string &value) const
{
  return rvalue (
    gcc_jit_context_new_string_literal (m_inner_ctxt, value.c_str ()));
}

inline rvalue
context::new_unary_op (enum gcc_jit_unary_op op,
		       type result_type,
		       rvalue a,
		       location loc)
{
  return rvalue (gcc_jit_context_new_unary_op (m_inner_ctxt,
					       loc.get_inner_location (),
					       op,
					       result_type.get_inner_type (),
					       a.get_inner_rvalue ()));
}
inline rvalue
context::new_minus (type result_type,
		    rvalue a,
		    location loc)
{
  return rvalue (new_unary_op (GCC_JIT_UNARY_OP_MINUS,
			       result_type, a, loc));
}
inline rvalue
context::new_bitwise_negate (type result_type,
			     rvalue a,
			     location loc)
{
  return rvalue (new_unary_op (GCC_JIT_UNARY_OP_BITWISE_NEGATE,
			       result_type, a, loc));
}
inline rvalue
context::new_logical_negate (type result_type,
			     rvalue a,
			     location loc)
{
  return rvalue (new_unary_op (GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
			       result_type, a, loc));
}

inline rvalue
context::new_binary_op (enum gcc_jit_binary_op op,
			type result_type,
			rvalue a, rvalue b,
			location loc)
{
  return rvalue (gcc_jit_context_new_binary_op (m_inner_ctxt,
						loc.get_inner_location (),
						op,
						result_type.get_inner_type (),
						a.get_inner_rvalue (),
						b.get_inner_rvalue ()));
}
inline rvalue
context::new_plus (type result_type,
		   rvalue a, rvalue b,
		   location loc)
{
  return new_binary_op (GCC_JIT_BINARY_OP_PLUS,
			result_type, a, b, loc);
}
inline rvalue
context::new_minus (type result_type,
		    rvalue a, rvalue b,
		    location loc)
{
  return new_binary_op (GCC_JIT_BINARY_OP_MINUS,
			result_type, a, b, loc);
}
inline rvalue
context::new_mult (type result_type,
		   rvalue a, rvalue b,
		   location loc)
{
  return new_binary_op (GCC_JIT_BINARY_OP_MULT,
			result_type, a, b, loc);
}
inline rvalue
context::new_divide (type result_type,
		     rvalue a, rvalue b,
		     location loc)
{
  return new_binary_op (GCC_JIT_BINARY_OP_DIVIDE,
			result_type, a, b, loc);
}
inline rvalue
context::new_modulo (type result_type,
		     rvalue a, rvalue b,
		     location loc)
{
  return new_binary_op (GCC_JIT_BINARY_OP_MODULO,
			result_type, a, b, loc);
}
inline rvalue
context::new_bitwise_and (type result_type,
			  rvalue a, rvalue b,
			  location loc)
{
  return new_binary_op (GCC_JIT_BINARY_OP_BITWISE_AND,
			result_type, a, b, loc);
}
inline rvalue
context::new_bitwise_xor (type result_type,
			  rvalue a, rvalue b,
			  location loc)
{
  return new_binary_op (GCC_JIT_BINARY_OP_BITWISE_XOR,
			result_type, a, b, loc);
}
inline rvalue
context::new_bitwise_or (type result_type,
			 rvalue a, rvalue b,
			 location loc)
{
  return new_binary_op (GCC_JIT_BINARY_OP_BITWISE_OR,
			result_type, a, b, loc);
}
inline rvalue
context::new_logical_and (type result_type,
			  rvalue a, rvalue b,
			  location loc)
{
  return new_binary_op (GCC_JIT_BINARY_OP_LOGICAL_AND,
			result_type, a, b, loc);
}
inline rvalue
context::new_logical_or (type result_type,
			 rvalue a, rvalue b,
			 location loc)
{
  return new_binary_op (GCC_JIT_BINARY_OP_LOGICAL_OR,
			result_type, a, b, loc);
}

inline rvalue
context::new_comparison (enum gcc_jit_comparison op,
			 rvalue a, rvalue b,
			 location loc)
{
  return rvalue (gcc_jit_context_new_comparison (m_inner_ctxt,
						 loc.get_inner_location (),
						 op,
						 a.get_inner_rvalue (),
						 b.get_inner_rvalue ()));
}
inline rvalue
context::new_eq (rvalue a, rvalue b,
		 location loc)
{
  return new_comparison (GCC_JIT_COMPARISON_EQ,
			 a, b, loc);
}
inline rvalue
context::new_ne (rvalue a, rvalue b,
		 location loc)
{
  return new_comparison (GCC_JIT_COMPARISON_NE,
			 a, b, loc);
}
inline rvalue
context::new_lt (rvalue a, rvalue b,
		 location loc)
{
  return new_comparison (GCC_JIT_COMPARISON_LT,
			 a, b, loc);
}
inline rvalue
context::new_le (rvalue a, rvalue b,
		 location loc)
{
  return new_comparison (GCC_JIT_COMPARISON_LE,
			 a, b, loc);
}
inline rvalue
context::new_gt (rvalue a, rvalue b,
		 location loc)
{
  return new_comparison (GCC_JIT_COMPARISON_GT,
			 a, b, loc);
}
inline rvalue
context::new_ge (rvalue a, rvalue b,
		 location loc)
{
  return new_comparison (GCC_JIT_COMPARISON_GE,
			 a, b, loc);
}

inline rvalue
context::new_call (function func,
		   std::vector<rvalue> &args,
		   location loc)
{
  /* Treat std::vector as an array, relying on it not being resized: */
  rvalue *as_array_of_wrappers = &args[0];

  /* Treat the array as being of the underlying pointers, relying on
     the wrapper type being such a pointer internally.	*/
  gcc_jit_rvalue **as_array_of_ptrs =
    reinterpret_cast<gcc_jit_rvalue **> (as_array_of_wrappers);
  return gcc_jit_context_new_call (m_inner_ctxt,
				   loc.get_inner_location (),
				   func.get_inner_function (),
				   args.size (),
				   as_array_of_ptrs);
}
inline rvalue
context::new_call (function func,
		   location loc)
{
  std::vector<rvalue> args;
  return new_call (func, args, loc);
}

inline rvalue
context::new_call (function func,
		   rvalue arg0,
		   location loc)
{
  std::vector<rvalue> args(1);
  args[0] = arg0;
  return new_call (func, args, loc);
}
inline rvalue
context::new_call (function func,
		   rvalue arg0, rvalue arg1,
		   location loc)
{
  std::vector<rvalue> args(2);
  args[0] = arg0;
  args[1] = arg1;
  return new_call (func, args, loc);
}
inline rvalue
context::new_call (function func,
		   rvalue arg0, rvalue arg1, rvalue arg2,
		   location loc)
{
  std::vector<rvalue> args(3);
  args[0] = arg0;
  args[1] = arg1;
  args[2] = arg2;
  return new_call (func, args, loc);
}
inline rvalue
context::new_call (function func,
		   rvalue arg0, rvalue arg1, rvalue arg2,
		   rvalue arg3,
		   location loc)
{
  std::vector<rvalue> args(4);
  args[0] = arg0;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  return new_call (func, args, loc);
}
inline rvalue
context::new_call (function func,
		   rvalue arg0, rvalue arg1, rvalue arg2,
		   rvalue arg3, rvalue arg4,
		   location loc)
{
  std::vector<rvalue> args(5);
  args[0] = arg0;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  args[4] = arg4;
  return new_call (func, args, loc);
}
inline rvalue
context::new_call (function func,
		   rvalue arg0, rvalue arg1, rvalue arg2,
		   rvalue arg3, rvalue arg4, rvalue arg5,
		   location loc)
{
  std::vector<rvalue> args(6);
  args[0] = arg0;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  args[4] = arg4;
  args[5] = arg5;
  return new_call (func, args, loc);
}

inline rvalue
context::new_cast (rvalue expr,
		   type type_,
		   location loc)
{
  return rvalue (gcc_jit_context_new_cast (m_inner_ctxt,
					   loc.get_inner_location (),
					   expr.get_inner_rvalue (),
					   type_.get_inner_type ()));
}

inline lvalue
context::new_array_access (rvalue ptr,
			   rvalue index,
			   location loc)
{
  return lvalue (gcc_jit_context_new_array_access (m_inner_ctxt,
						   loc.get_inner_location (),
						   ptr.get_inner_rvalue (),
						   index.get_inner_rvalue ()));
}

inline case_
context::new_case (rvalue min_value,
		   rvalue max_value,
		   block dest_block)
{
  return case_ (gcc_jit_context_new_case (m_inner_ctxt,
					  min_value.get_inner_rvalue (),
					  max_value.get_inner_rvalue (),
					  dest_block.get_inner_block ()));
}

// class object
inline context
object::get_context () const
{
  return context (gcc_jit_object_get_context (m_inner_obj));
}

inline std::string
object::get_debug_string () const
{
  return gcc_jit_object_get_debug_string (m_inner_obj);
}

inline object::object () : m_inner_obj (NULL) {}
inline object::object (gcc_jit_object *obj) : m_inner_obj (obj)
{
  if (!obj)
    throw error ();
}

inline gcc_jit_object *
object::get_inner_object () const
{
  return m_inner_obj;
}

inline std::ostream&
operator << (std::ostream& stream, const object &obj)
{
  return stream << obj.get_debug_string ();
}

// class location
inline location::location () : object () {}
inline location::location (gcc_jit_location *loc)
  : object (gcc_jit_location_as_object (loc))
{}

inline gcc_jit_location *
location::get_inner_location () const
{
  /* Manual downcast: */
  return reinterpret_cast<gcc_jit_location *> (get_inner_object ());
}

// class field
inline field::field () : object () {}
inline field::field (gcc_jit_field *inner)
  : object (gcc_jit_field_as_object (inner))
{}

inline gcc_jit_field *
field::get_inner_field () const
{
  /* Manual downcast: */
  return reinterpret_cast<gcc_jit_field *> (get_inner_object ());
}

// class type
inline type::type () : object () {}
inline type::type (gcc_jit_type *inner)
  : object (gcc_jit_type_as_object (inner))
{}

inline gcc_jit_type *
type::get_inner_type () const
{
  /* Manual downcast: */
  return reinterpret_cast<gcc_jit_type *> (get_inner_object ());
}

inline type
type::get_pointer ()
{
  return type (gcc_jit_type_get_pointer (get_inner_type ()));
}

inline type
type::get_const ()
{
  return type (gcc_jit_type_get_const (get_inner_type ()));
}

inline type
type::get_volatile ()
{
  return type (gcc_jit_type_get_volatile (get_inner_type ()));
}

inline type
type::get_aligned (size_t alignment_in_bytes)
{
  return type (gcc_jit_type_get_aligned (get_inner_type (),
					 alignment_in_bytes));
}

inline type
type::get_vector (size_t num_units)
{
  return type (gcc_jit_type_get_vector (get_inner_type (),
					num_units));
}

inline rvalue
type::zero ()
{
  return get_context ().new_rvalue (*this, 0);
}

inline rvalue
type::one ()
{
  return get_context ().new_rvalue (*this, 1);
}

// class struct_
inline struct_::struct_ () : type (NULL) {}
inline struct_::struct_ (gcc_jit_struct *inner) :
  type (gcc_jit_struct_as_type (inner))
{
}

inline gcc_jit_struct *
struct_::get_inner_struct () const
{
  /* Manual downcast: */
  return reinterpret_cast<gcc_jit_struct *> (get_inner_object ());
}

// class function
inline function::function () : object () {}
inline function::function (gcc_jit_function *inner)
  : object (gcc_jit_function_as_object (inner))
{}

inline gcc_jit_function *
function::get_inner_function () const
{
  /* Manual downcast: */
  return reinterpret_cast<gcc_jit_function *> (get_inner_object ());
}

inline void
function::dump_to_dot (const std::string &path)
{
  gcc_jit_function_dump_to_dot (get_inner_function (),
				path.c_str ());
}

inline param
function::get_param (int index) const
{
  return param (gcc_jit_function_get_param (get_inner_function (),
					    index));
}

inline block
function::new_block ()
{
  return block (gcc_jit_function_new_block (get_inner_function (),
					    NULL));
}

inline block
function::new_block (const std::string &name)
{
  return block (gcc_jit_function_new_block (get_inner_function (),
					    name.c_str ()));
}

inline lvalue
function::new_local (type type_,
		     const std::string &name,
		     location loc)
{
  return lvalue (gcc_jit_function_new_local (get_inner_function (),
					     loc.get_inner_location (),
					     type_.get_inner_type (),
					     name.c_str ()));
}

inline rvalue
function::get_address (location loc)
{
  return rvalue (gcc_jit_function_get_address (get_inner_function (),
					       loc.get_inner_location ()));
}

inline function
block::get_function () const
{
  return function (gcc_jit_block_get_function ( get_inner_block ()));
}

inline void
block::add_eval (rvalue rvalue,
		 location loc)
{
  gcc_jit_block_add_eval (get_inner_block (),
			  loc.get_inner_location (),
			  rvalue.get_inner_rvalue ());
}

inline void
block::add_assignment (lvalue lvalue,
		       rvalue rvalue,
		       location loc)
{
  gcc_jit_block_add_assignment (get_inner_block (),
				loc.get_inner_location (),
				lvalue.get_inner_lvalue (),
				rvalue.get_inner_rvalue ());
}

inline void
block::add_assignment_op (lvalue lvalue,
			  enum gcc_jit_binary_op op,
			  rvalue rvalue,
			  location loc)
{
  gcc_jit_block_add_assignment_op (get_inner_block (),
				   loc.get_inner_location (),
				   lvalue.get_inner_lvalue (),
				   op,
				   rvalue.get_inner_rvalue ());
}

inline void
block::add_comment (const std::string &text,
		    location loc)
{
  gcc_jit_block_add_comment (get_inner_block (),
			     loc.get_inner_location (),
			     text.c_str ());
}

inline void
block::end_with_conditional (rvalue boolval,
			     block on_true,
			     block on_false,
			     location loc)
{
  gcc_jit_block_end_with_conditional (get_inner_block (),
				      loc.get_inner_location (),
				      boolval.get_inner_rvalue (),
				      on_true.get_inner_block (),
				      on_false.get_inner_block ());
}

inline void
block::end_with_jump (block target,
		      location loc)
{
  gcc_jit_block_end_with_jump (get_inner_block (),
			       loc.get_inner_location (),
			       target.get_inner_block ());
}

inline void
block::end_with_return (rvalue rvalue,
			location loc)
{
  gcc_jit_block_end_with_return (get_inner_block (),
				 loc.get_inner_location (),
				 rvalue.get_inner_rvalue ());
}

inline void
block::end_with_return (location loc)
{
  gcc_jit_block_end_with_void_return (get_inner_block (),
				      loc.get_inner_location ());
}

inline void
block::end_with_switch (rvalue expr,
			block default_block,
			std::vector <case_> cases,
			location loc)
{
  /* Treat std::vector as an array, relying on it not being resized: */
  case_ *as_array_of_wrappers = &cases[0];

  /* Treat the array as being of the underlying pointers, relying on
     the wrapper type being such a pointer internally.	*/
  gcc_jit_case **as_array_of_ptrs =
    reinterpret_cast<gcc_jit_case **> (as_array_of_wrappers);
  gcc_jit_block_end_with_switch (get_inner_block (),
				 loc.get_inner_location (),
				 expr.get_inner_rvalue (),
				 default_block.get_inner_block (),
				 cases.size (),
				 as_array_of_ptrs);
}

inline rvalue
block::add_call (function other,
		 location loc)
{
  rvalue c = get_context ().new_call (other, loc);
  add_eval (c);
  return c;
}
inline rvalue
block::add_call (function other,
		 rvalue arg0,
		 location loc)
{
  rvalue c = get_context ().new_call (other, arg0, loc);
  add_eval (c);
  return c;
}
inline rvalue
block::add_call (function other,
		 rvalue arg0, rvalue arg1,
		 location loc)
{
  rvalue c = get_context ().new_call (other, arg0, arg1, loc);
  add_eval (c);
  return c;
}
inline rvalue
block::add_call (function other,
		 rvalue arg0, rvalue arg1, rvalue arg2,
		 location loc)
{
  rvalue c = get_context ().new_call (other, arg0, arg1, arg2, loc);
  add_eval (c);
  return c;
}

inline rvalue
block::add_call (function other,
		 rvalue arg0, rvalue arg1, rvalue arg2, rvalue arg3,
		 location loc)
{
  rvalue c = get_context ().new_call (other, arg0, arg1, arg2, arg3, loc);
  add_eval (c);
  return c;
}

inline rvalue
function::operator() (location loc)
{
  return get_context ().new_call (*this, loc);
}
inline rvalue
function::operator() (rvalue arg0,
		      location loc)
{
  return get_context ().new_call (*this,
				  arg0,
				  loc);
}
inline rvalue
function::operator() (rvalue arg0, rvalue arg1,
		      location loc)
{
  return get_context ().new_call (*this,
				  arg0, arg1,
				  loc);
}
inline rvalue
function::operator() (rvalue arg0, rvalue arg1, rvalue arg2,
		      location loc)
{
  return get_context ().new_call (*this,
				  arg0, arg1, arg2,
				  loc);
}

// class block
inline block::block () : object () {}
inline block::block (gcc_jit_block *inner)
  : object (gcc_jit_block_as_object (inner))
{}

inline gcc_jit_block *
block::get_inner_block () const
{
  /* Manual downcast: */
  return reinterpret_cast<gcc_jit_block *> (get_inner_object ());
}

//  class rvalue
inline rvalue::rvalue () : object () {}
inline rvalue::rvalue (gcc_jit_rvalue *inner)
  : object (gcc_jit_rvalue_as_object (inner))
{}

inline gcc_jit_rvalue *
rvalue::get_inner_rvalue () const
{
  /* Manual downcast: */
  return reinterpret_cast<gcc_jit_rvalue *> (get_inner_object ());
}

inline type
rvalue::get_type ()
{
  return type (gcc_jit_rvalue_get_type (get_inner_rvalue ()));
}

inline rvalue
rvalue::access_field (field field,
		      location loc)
{
  return rvalue (gcc_jit_rvalue_access_field (get_inner_rvalue (),
					      loc.get_inner_location (),
					      field.get_inner_field ()));
}

inline lvalue
rvalue::dereference_field (field field,
			   location loc)
{
  return lvalue (gcc_jit_rvalue_dereference_field (get_inner_rvalue (),
						   loc.get_inner_location (),
						   field.get_inner_field ()));
}

inline lvalue
rvalue::dereference (location loc)
{
  return lvalue (gcc_jit_rvalue_dereference (get_inner_rvalue (),
					     loc.get_inner_location ()));
}

inline rvalue
rvalue::cast_to (type type_,
		 location loc)
{
  return get_context ().new_cast (*this, type_, loc);
}

inline lvalue
rvalue::operator[] (rvalue index)
{
  return get_context ().new_array_access (*this, index);
}

inline lvalue
rvalue::operator[] (int index)
{
  context ctxt = get_context ();
  type int_t = ctxt.get_int_type <int> ();
  return ctxt.new_array_access (*this,
				ctxt.new_rvalue (int_t,
						 index));
}

// class lvalue : public rvalue
inline lvalue::lvalue () : rvalue () {}
inline lvalue::lvalue (gcc_jit_lvalue *inner)
  : rvalue (gcc_jit_lvalue_as_rvalue (inner))
{}

inline gcc_jit_lvalue *
lvalue::get_inner_lvalue () const
{
  /* Manual downcast: */
  return reinterpret_cast<gcc_jit_lvalue *> (get_inner_object ());
}

inline lvalue
lvalue::access_field (field field, location loc)
{
  return lvalue (gcc_jit_lvalue_access_field (get_inner_lvalue (),
					      loc.get_inner_location (),
					      field.get_inner_field ()));
}

inline rvalue
lvalue::get_address (location loc)
{
  return rvalue (gcc_jit_lvalue_get_address (get_inner_lvalue (),
					     loc.get_inner_location ()));
}

// class param : public lvalue
inline param::param () : lvalue () {}
inline param::param (gcc_jit_param *inner)
  : lvalue (gcc_jit_param_as_lvalue (inner))
{}

// class case_ : public object
inline case_::case_ () : object () {}
inline case_::case_ (gcc_jit_case *inner)
  : object (gcc_jit_case_as_object (inner))
{
}

inline gcc_jit_case *
case_::get_inner_case () const
{
  /* Manual downcast: */
  return reinterpret_cast<gcc_jit_case *> (get_inner_object ());
}

/* Overloaded operators.  */
// Unary operators
inline rvalue operator- (rvalue a)
{
  return a.get_context ().new_minus (a.get_type (), a);
}
inline rvalue operator~ (rvalue a)
{
  return a.get_context ().new_bitwise_negate (a.get_type (), a);
}
inline rvalue operator! (rvalue a)
{
  return a.get_context ().new_logical_negate (a.get_type (), a);
}

// Binary operators
inline rvalue operator+ (rvalue a, rvalue b)
{
  return a.get_context ().new_plus (a.get_type (), a, b);
}
inline rvalue operator- (rvalue a, rvalue b)
{
  return a.get_context ().new_minus (a.get_type (), a, b);
}
inline rvalue operator* (rvalue a, rvalue b)
{
  return a.get_context ().new_mult (a.get_type (), a, b);
}
inline rvalue operator/ (rvalue a, rvalue b)
{
  return a.get_context ().new_divide (a.get_type (), a, b);
}
inline rvalue operator% (rvalue a, rvalue b)
{
  return a.get_context ().new_modulo (a.get_type (), a, b);
}
inline rvalue operator& (rvalue a, rvalue b)
{
  return a.get_context ().new_bitwise_and (a.get_type (), a, b);
}
inline rvalue operator^ (rvalue a, rvalue b)
{
  return a.get_context ().new_bitwise_xor (a.get_type (), a, b);
}
inline rvalue operator| (rvalue a, rvalue b)
{
  return a.get_context ().new_bitwise_or (a.get_type (), a, b);
}
inline rvalue operator&& (rvalue a, rvalue b)
{
  return a.get_context ().new_logical_and (a.get_type (), a, b);
}
inline rvalue operator|| (rvalue a, rvalue b)
{
  return a.get_context ().new_logical_or (a.get_type (), a, b);
}

/* Comparisons.  */
inline rvalue operator== (rvalue a, rvalue b)
{
  return a.get_context ().new_eq (a, b);
}
inline rvalue operator!= (rvalue a, rvalue b)
{
  return a.get_context ().new_ne (a, b);
}
inline rvalue operator< (rvalue a, rvalue b)
{
  return a.get_context ().new_lt (a, b);
}
inline rvalue operator<= (rvalue a, rvalue b)
{
  return a.get_context ().new_le (a, b);
}
inline rvalue operator> (rvalue a, rvalue b)
{
  return a.get_context ().new_gt (a, b);
}
inline rvalue operator>= (rvalue a, rvalue b)
{
  return a.get_context ().new_ge (a, b);
}

/* Dereferencing. */
inline lvalue operator* (rvalue ptr)
{
  return ptr.dereference ();
}

// class timer
inline
timer::timer ()
{
  m_inner_timer = gcc_jit_timer_new ();
}

inline
timer::timer (gcc_jit_timer *inner_timer)
{
  m_inner_timer = inner_timer;
}

inline void
timer::push (const char *item_name)
{
  gcc_jit_timer_push (m_inner_timer, item_name);

}

inline void
timer::pop (const char *item_name)
{
  gcc_jit_timer_pop (m_inner_timer, item_name);
}

inline void
timer::print (FILE *f_out) const
{
  gcc_jit_timer_print (m_inner_timer, f_out);
}

inline gcc_jit_timer *
timer::get_inner_timer () const
{
  return m_inner_timer;
}

inline void
timer::release ()
{
  gcc_jit_timer_release (m_inner_timer);
  m_inner_timer = NULL;
}

// class auto_time

inline
auto_time::auto_time (timer t, const char *item_name)
  : m_timer (t),
    m_item_name (item_name)
{
  t.push (item_name);
}

inline
auto_time::auto_time (context ctxt, const char *item_name)
  : m_timer (ctxt.get_timer ()),
    m_item_name (item_name)
{
  m_timer.push (item_name);
}

inline
auto_time::~auto_time ()
{
  m_timer.pop (m_item_name);
}

} // namespace gccjit

#endif /* #ifndef LIBGCCJIT_PLUS_PLUS_H */
