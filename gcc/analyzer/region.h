/* Regions of memory.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_REGION_H
#define GCC_ANALYZER_REGION_H

#include "analyzer/symbol.h"

namespace ana {

/* An enum for identifying different spaces within memory.  */

enum memory_space
{
  MEMSPACE_UNKNOWN,
  MEMSPACE_CODE,
  MEMSPACE_GLOBALS,
  MEMSPACE_STACK,
  MEMSPACE_HEAP,
  MEMSPACE_READONLY_DATA,
  MEMSPACE_THREAD_LOCAL,
  MEMSPACE_PRIVATE
};

/* An enum for discriminating between the different concrete subclasses
   of region.  */

enum region_kind
{
  RK_FRAME,
  RK_GLOBALS,
  RK_CODE,
  RK_FUNCTION,
  RK_LABEL,
  RK_STACK,
  RK_HEAP,
  RK_THREAD_LOCAL,
  RK_ROOT,
  RK_SYMBOLIC,
  RK_DECL,
  RK_FIELD,
  RK_ELEMENT,
  RK_OFFSET,
  RK_SIZED,
  RK_CAST,
  RK_HEAP_ALLOCATED,
  RK_ALLOCA,
  RK_STRING,
  RK_BIT_RANGE,
  RK_VAR_ARG,
  RK_ERRNO,
  RK_PRIVATE,
  RK_UNKNOWN,
};

/* Region and its subclasses.

   The class hierarchy looks like this (using indentation to show
   inheritance, and with region_kinds shown for the concrete subclasses):

   region
     space_region
       frame_region (RK_FRAME): a function frame on the stack
       globals_region (RK_GLOBALS): holds globals variables (data and bss)
       code_region (RK_CODE): represents the code segment, containing functions
       stack_region (RK_STACK): a stack, containing all stack frames
       heap_region (RK_HEAP): the heap, containing heap_allocated_regions
       thread_local_region (RK_THREAD_LOCAL): thread-local data for the thread
                                              being analyzed
     root_region (RK_ROOT): the top-level region
     function_region (RK_FUNCTION): the code for a particular function
     label_region (RK_LABEL): a particular label within a function
     symbolic_region (RK_SYMBOLIC): dereferencing a symbolic pointer
     decl_region (RK_DECL): the memory occupied by a particular global, local,
			    or SSA name
     field_region (RK_FIELD): the memory occupied by a field within a struct
			      or union
     element_region (RK_ELEMENT): an element within an array
     offset_region (RK_OFFSET): a byte-offset within another region, for
				handling pointer arithmetic as a region
     sized_region (RK_SIZED): a subregion of symbolic size (in bytes)
			      within its parent
     cast_region (RK_CAST): a region that views another region using a
			    different type
     heap_allocated_region (RK_HEAP_ALLOCATED): an untyped region dynamically
						allocated on the heap via
						"malloc" or similar
     alloca_region (RK_ALLOCA): an untyped region dynamically allocated on the
				stack via "alloca"
     string_region (RK_STRING): a region for a STRING_CST
     bit_range_region (RK_BIT_RANGE): a region for a specific range of bits
				      within another region
     var_arg_region (RK_VAR_ARG): a region for the N-th vararg within a
				  frame_region for a variadic call
     errno_region (RK_ERRNO): a region for holding "errno"
     private_region (RK_PRIVATE): a region for internal state of an API
     unknown_region (RK_UNKNOWN): for handling unimplemented tree codes.  */

/* Abstract base class for representing ways of accessing chunks of memory.

   Regions form a tree-like hierarchy, with a root region at the base,
   with memory space regions within it, representing the stack and
   globals, with frames within the stack, and regions for variables
   within the frames and the "globals" region.  Regions for structs
   can have subregions for fields.  */

class region : public symbol
{
public:
  virtual ~region ();

  virtual enum region_kind get_kind () const = 0;
  virtual const frame_region *
  dyn_cast_frame_region () const { return NULL; }
  virtual const function_region *
  dyn_cast_function_region () const { return NULL; }
  virtual const symbolic_region *
  dyn_cast_symbolic_region () const { return NULL; }
  virtual const decl_region *
  dyn_cast_decl_region () const { return NULL; }
  virtual const field_region *
  dyn_cast_field_region () const { return NULL; }
  virtual const element_region *
  dyn_cast_element_region () const { return NULL; }
  virtual const offset_region *
  dyn_cast_offset_region () const { return NULL; }
  virtual const sized_region *
  dyn_cast_sized_region () const { return NULL; }
  virtual const cast_region *
  dyn_cast_cast_region () const { return NULL; }
  virtual const string_region *
  dyn_cast_string_region () const { return NULL; }
  virtual const bit_range_region *
  dyn_cast_bit_range_region () const { return NULL; }
  virtual const var_arg_region *
  dyn_cast_var_arg_region () const { return NULL; }

  virtual void accept (visitor *v) const;

  const region *get_parent_region () const { return m_parent; }
  const region *get_base_region () const;
  bool base_region_p () const;
  bool descendent_of_p (const region *elder) const;
  const frame_region *maybe_get_frame_region () const;
  enum memory_space get_memory_space () const;
  bool can_have_initial_svalue_p () const;
  const svalue *get_initial_value_at_main (region_model_manager *mgr) const;

  tree maybe_get_decl () const;

  tree get_type () const { return m_type; }

  void print (const region_model &model,
	      pretty_printer *pp) const;
  label_text get_desc (bool simple=true) const;

  virtual void dump_to_pp (pretty_printer *pp, bool simple) const = 0;
  void dump (bool simple) const;

  json::value *to_json () const;

  bool non_null_p () const;

  static int cmp_ptr_ptr (const void *, const void *);

  bool involves_p (const svalue *sval) const;

  region_offset get_offset (region_model_manager *mgr) const;
  region_offset get_next_offset (region_model_manager *mgr) const;

  /* Attempt to get the size of this region as a concrete number of bytes.
     If successful, return true and write the size to *OUT.
     Otherwise return false.  */
  virtual bool get_byte_size (byte_size_t *out) const;

  /* Attempt to get the size of this region as a concrete number of bits.
     If successful, return true and write the size to *OUT.
     Otherwise return false.  */
  virtual bool get_bit_size (bit_size_t *out) const;

  /* Get a symbolic value describing the size of this region in bytes
     (which could be "unknown").  */
  virtual const svalue *get_byte_size_sval (region_model_manager *mgr) const;

  /* Get a symbolic value describing the size of this region in bits
     (which could be "unknown").  */
  virtual const svalue *get_bit_size_sval (region_model_manager *mgr) const;

  /* Attempt to get the offset in bits of this region relative to its parent.
     If successful, return true and write to *OUT.
     Otherwise return false.  */
  virtual bool get_relative_concrete_offset (bit_offset_t *out) const;

  /* Get the offset in bytes of this region relative to its parent as a svalue.
     Might return an unknown_svalue.  */
  virtual const svalue *
  get_relative_symbolic_offset (region_model_manager *mgr) const;

  /* Attempt to get the position and size of this region expressed as a
     concrete range of bytes relative to its parent.
     If successful, return true and write to *OUT.
     Otherwise return false.  */
  bool get_relative_concrete_byte_range (byte_range *out) const;

  void
  get_subregions_for_binding (region_model_manager *mgr,
			      bit_offset_t start_bit_offset,
			      bit_size_t size_in_bits,
			      tree type,
			      auto_vec <const region *> *out) const;

  bool symbolic_for_unknown_ptr_p () const;

  bool symbolic_p () const;

  /* For most base regions it makes sense to track the bindings of the region
     within the store.  As an optimization, some are not tracked (to avoid
     bloating the store object with redundant binding clusters).  */
  virtual bool tracked_p () const { return true; }

  bool is_named_decl_p (const char *decl_name) const;

  bool empty_p () const;

 protected:
  region (complexity c, symbol::id_t id, const region *parent, tree type);

 private:
  region_offset calc_offset (region_model_manager *mgr) const;
  const svalue *calc_initial_value_at_main (region_model_manager *mgr) const;

  const region *m_parent;
  tree m_type;

  mutable region_offset *m_cached_offset;

  /* For regions within a global decl, a cache of the svalue for the initial
     value of this region when the program starts.  */
  mutable const svalue *m_cached_init_sval_at_main;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const region *>::test (const region *)
{
  return true;
}

namespace ana {

/* Abstract subclass of region, for regions that represent an untyped
   space within memory, such as the stack or the heap.  */

class space_region : public region
{
protected:
  space_region (symbol::id_t id, const region *parent)
  : region (complexity (parent), id, parent, NULL_TREE)
  {}
};

/* Concrete space_region subclass, representing a function frame on the stack,
   to contain the locals.
   The parent is the stack region; there's also a hierarchy of call-stack
   prefixes expressed via m_calling_frame.
   For example, given "oldest" calling "middle" called "newest" we would have
   - a stack depth of 3
   - frame (A) for "oldest" with index 0 for depth 1, calling_frame == NULL
   - frame (B) for "middle" with index 1 for depth 2, calling_frame == (A)
   - frame (C) for "newest" with index 2 for depth 3, calling_frame == (B)
   where the parent region for each of the frames is the "stack" region.
   The index is the count of frames earlier than this in the stack.  */

class frame_region : public space_region
{
public:
  /* A support class for uniquifying instances of frame_region.  */
  struct key_t
  {
    key_t (const frame_region *calling_frame, function *fun)
    : m_calling_frame (calling_frame), m_fun (fun)
    {
      /* calling_frame can be NULL.  */
      gcc_assert (fun);
    }

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_calling_frame);
      hstate.add_ptr (m_fun);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_calling_frame == other.m_calling_frame && m_fun == other.m_fun);
    }

    void mark_deleted () { m_fun = reinterpret_cast<function *> (1); }
    void mark_empty () { m_fun = NULL; }
    bool is_deleted () const
    {
      return m_fun == reinterpret_cast<function *> (1);
    }
    bool is_empty () const { return m_fun == NULL; }

    const frame_region *m_calling_frame;
    function *m_fun;
  };

  frame_region (symbol::id_t id, const region *parent,
		const frame_region *calling_frame,
		function *fun, int index)
  : space_region (id, parent), m_calling_frame (calling_frame),
    m_fun (fun), m_index (index)
  {}
  ~frame_region ();

  /* region vfuncs.  */
  enum region_kind get_kind () const final override { return RK_FRAME; }
  const frame_region * dyn_cast_frame_region () const final override
  {
    return this;
  }
  void accept (visitor *v) const final override;
  void dump_to_pp (pretty_printer *pp, bool simple) const final override;

  /* Accessors.  */
  const frame_region *get_calling_frame () const { return m_calling_frame; }
  function *get_function () const { return m_fun; }
  tree get_fndecl () const { return get_function ()->decl; }
  int get_index () const { return m_index; }
  int get_stack_depth () const { return m_index + 1; }

  const decl_region *
  get_region_for_local (region_model_manager *mgr,
			tree expr,
			const region_model_context *ctxt) const;

  unsigned get_num_locals () const { return m_locals.elements (); }

  /* Implemented in region-model-manager.cc.  */
  void dump_untracked_regions () const;

 private:
  const frame_region *m_calling_frame;
  function *m_fun;
  int m_index;

  /* The regions for the decls within this frame are managed by this
     object, rather than the region_model_manager, to make it a simple
     lookup by tree.  */
  typedef hash_map<tree, decl_region *> map_t;
  map_t m_locals;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const frame_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_FRAME;
}

template <> struct default_hash_traits<frame_region::key_t>
: public member_function_hash_traits<frame_region::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* Concrete space_region subclass, to hold global variables (data and bss).  */

class globals_region : public space_region
{
 public:
  globals_region (symbol::id_t id, const region *parent)
  : space_region (id, parent)
  {}

  /* region vfuncs.  */
  enum region_kind get_kind () const final override { return RK_GLOBALS; }
  void dump_to_pp (pretty_printer *pp, bool simple) const final override;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const globals_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_GLOBALS;
}

namespace ana {

/* Concrete space_region subclass, representing the code segment
   containing functions.  */

class code_region : public space_region
{
public:
  code_region (symbol::id_t id, const region *parent)
  : space_region (id, parent)
  {}

  /* region vfuncs.  */
  void dump_to_pp (pretty_printer *pp, bool simple) const final override;
  enum region_kind get_kind () const final override { return RK_CODE; }
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const code_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_CODE;
}

namespace ana {

/* Concrete region subclass.  A region representing the code for
   a particular function.  */

class function_region : public region
{
public:
  function_region (symbol::id_t id, const code_region *parent, tree fndecl)
  : region (complexity (parent), id, parent, TREE_TYPE (fndecl)),
    m_fndecl (fndecl)
  {
    gcc_assert (FUNC_OR_METHOD_TYPE_P (TREE_TYPE (fndecl)));
  }

  /* region vfuncs.  */
  void dump_to_pp (pretty_printer *pp, bool simple) const final override;
  enum region_kind get_kind () const final override { return RK_FUNCTION; }
  const function_region *
  dyn_cast_function_region () const final override{ return this; }

  tree get_fndecl () const { return m_fndecl; }

private:
  tree m_fndecl;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const function_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_FUNCTION;
}

namespace ana {

/* Concrete region subclass.  A region representing a particular label
   within a function.  */

class label_region : public region
{
public:
  label_region (symbol::id_t id, const function_region *parent, tree label)
  : region (complexity (parent), id, parent, NULL_TREE), m_label (label)
  {
    gcc_assert (TREE_CODE (label) == LABEL_DECL);
  }

  /* region vfuncs.  */
  void dump_to_pp (pretty_printer *pp, bool simple) const final override;
  enum region_kind get_kind () const final override { return RK_LABEL; }

  tree get_label () const { return m_label; }

private:
  tree m_label;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const label_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_LABEL;
}

namespace ana {

/* Concrete space_region subclass representing a stack, containing all stack
   frames.  */

class stack_region : public space_region
{
public:
  stack_region (symbol::id_t id, region *parent)
  : space_region (id, parent)
  {}

  void dump_to_pp (pretty_printer *pp, bool simple) const final override;

  enum region_kind get_kind () const final override { return RK_STACK; }
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const stack_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_STACK;
}

namespace ana {

/* Concrete space_region subclass: a region within which regions can be
   dynamically allocated.  */

class heap_region : public space_region
{
public:
  heap_region (symbol::id_t id, region *parent)
  : space_region (id, parent)
  {}

  enum region_kind get_kind () const final override { return RK_HEAP; }
  void dump_to_pp (pretty_printer *pp, bool simple) const final override;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const heap_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_HEAP;
}

namespace ana {

/* Concrete space_region subclass: thread-local data for the thread
   being analyzed.  */

class thread_local_region : public space_region
{
public:
  thread_local_region (symbol::id_t id, region *parent)
  : space_region (id, parent)
  {}

  enum region_kind get_kind () const final override { return RK_THREAD_LOCAL; }
  void dump_to_pp (pretty_printer *pp, bool simple) const final override;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const thread_local_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_THREAD_LOCAL;
}

namespace ana {

/* Concrete region subclass.  The root region, containing all regions
   (either directly, or as descendents).
   Unique within a region_model_manager.  */

class root_region : public region
{
public:
  root_region (symbol::id_t id);

  enum region_kind get_kind () const final override { return RK_ROOT; }
  void dump_to_pp (pretty_printer *pp, bool simple) const final override;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const root_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_ROOT;
}

namespace ana {

/* Concrete region subclass: a region to use when dereferencing an unknown
   pointer.  */

class symbolic_region : public region
{
public:
  /* A support class for uniquifying instances of symbolic_region.  */
  struct key_t
  {
    key_t (const region *parent, const svalue *sval_ptr)
    : m_parent (parent), m_sval_ptr (sval_ptr)
    {
      gcc_assert (sval_ptr);
    }

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_parent);
      hstate.add_ptr (m_sval_ptr);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_parent == other.m_parent && m_sval_ptr == other.m_sval_ptr);
    }

    void mark_deleted () { m_sval_ptr = reinterpret_cast<const svalue *> (1); }
    void mark_empty () { m_sval_ptr = NULL; }
    bool is_deleted () const
    {
      return m_sval_ptr == reinterpret_cast<const svalue *> (1);
    }
    bool is_empty () const { return m_sval_ptr == NULL; }

    const region *m_parent;
    const svalue *m_sval_ptr;
  };

  symbolic_region (symbol::id_t id, region *parent, const svalue *sval_ptr);

  const symbolic_region *
  dyn_cast_symbolic_region () const final override { return this; }

  enum region_kind get_kind () const final override { return RK_SYMBOLIC; }
  void accept (visitor *v) const final override;
  void dump_to_pp (pretty_printer *pp, bool simple) const final override;

  const svalue *get_pointer () const { return m_sval_ptr; }

private:
  const svalue *m_sval_ptr;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const symbolic_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_SYMBOLIC;
}

template <> struct default_hash_traits<symbolic_region::key_t>
: public member_function_hash_traits<symbolic_region::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* Concrete region subclass representing the memory occupied by a
   variable (whether for a global or a local).
   Also used for representing SSA names, as if they were locals.  */

class decl_region : public region
{
public:
  decl_region (symbol::id_t id, const region *parent, tree decl)
  : region (complexity (parent), id, parent, TREE_TYPE (decl)), m_decl (decl),
    m_tracked (calc_tracked_p (decl)),
    m_ctor_svalue (NULL)
  {}

  enum region_kind get_kind () const final override { return RK_DECL; }
  const decl_region *
  dyn_cast_decl_region () const final override { return this; }

  void dump_to_pp (pretty_printer *pp, bool simple) const final override;

  bool tracked_p () const final override { return m_tracked; }

  tree get_decl () const { return m_decl; }
  int get_stack_depth () const;

  const svalue *maybe_get_constant_value (region_model_manager *mgr) const;
  const svalue *get_svalue_for_constructor (tree ctor,
					    region_model_manager *mgr) const;
  const svalue *get_svalue_for_initializer (region_model_manager *mgr) const;

private:
  const svalue *calc_svalue_for_constructor (tree ctor,
					     region_model_manager *mgr) const;
  static bool calc_tracked_p (tree decl);

  tree m_decl;

  /* Cached result of calc_tracked_p, so that we can quickly determine when
     we don't to track a binding_cluster for this decl (to avoid bloating
     store objects).
     This can be debugged using -fdump-analyzer-untracked.  */
  bool m_tracked;

  /* Cached result of get_svalue_for_constructor.  */
  mutable const svalue *m_ctor_svalue;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const decl_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_DECL;
}

namespace ana {

/* Concrete region subclass representing the memory occupied by a
   field within a struct or union.  */

class field_region : public region
{
public:
  /* A support class for uniquifying instances of field_region.  */
  struct key_t
  {
    key_t (const region *parent, tree field)
    : m_parent (parent), m_field (field)
    {
      gcc_assert (field);
    }

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_parent);
      hstate.add_ptr (m_field);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_parent == other.m_parent && m_field == other.m_field);
    }

    void mark_deleted () { m_field = reinterpret_cast<tree> (1); }
    void mark_empty () { m_field = NULL_TREE; }
    bool is_deleted () const { return m_field == reinterpret_cast<tree> (1); }
    bool is_empty () const { return m_field == NULL_TREE; }

    const region *m_parent;
    tree m_field;
  };

  field_region (symbol::id_t id, const region *parent, tree field)
  : region (complexity (parent), id, parent, TREE_TYPE (field)),
    m_field (field)
  {}

  enum region_kind get_kind () const final override { return RK_FIELD; }

  void dump_to_pp (pretty_printer *pp, bool simple) const final override;
  const field_region *
  dyn_cast_field_region () const final override { return this; }

  tree get_field () const { return m_field; }

  bool get_relative_concrete_offset (bit_offset_t *out) const final override;
  const svalue *get_relative_symbolic_offset (region_model_manager *mgr)
    const final override;

private:
  tree m_field;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const field_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_FIELD;
}

template <> struct default_hash_traits<field_region::key_t>
: public member_function_hash_traits<field_region::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* An element within an array.  */

class element_region : public region
{
public:
  /* A support class for uniquifying instances of element_region.  */
  struct key_t
  {
    key_t (const region *parent, tree element_type, const svalue *index)
    : m_parent (parent), m_element_type (element_type), m_index (index)
    {
      gcc_assert (index);
    }

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_parent);
      hstate.add_ptr (m_element_type);
      hstate.add_ptr (m_index);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_parent == other.m_parent
	      && m_element_type == other.m_element_type
	      && m_index == other.m_index);
    }

    void mark_deleted () { m_index = reinterpret_cast<const svalue *> (1); }
    void mark_empty () { m_index = NULL; }
    bool is_deleted () const
    {
      return m_index == reinterpret_cast<const svalue *> (1);
    }
    bool is_empty () const { return m_index == NULL; }

    const region *m_parent;
    tree m_element_type;
    const svalue *m_index;
  };

  element_region (symbol::id_t id, const region *parent, tree element_type,
		  const svalue *index)
  : region (complexity::from_pair (parent, index), id, parent, element_type),
    m_index (index)
  {}

  enum region_kind get_kind () const final override { return RK_ELEMENT; }
  const element_region *
  dyn_cast_element_region () const final override { return this; }

  void accept (visitor *v) const final override;

  void dump_to_pp (pretty_printer *pp, bool simple) const final override;

  const svalue *get_index () const { return m_index; }

  virtual bool
  get_relative_concrete_offset (bit_offset_t *out) const final override;
  const svalue *get_relative_symbolic_offset (region_model_manager *mgr)
    const final override;

private:
  const svalue *m_index;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const element_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_ELEMENT;
}

template <> struct default_hash_traits<element_region::key_t>
: public member_function_hash_traits<element_region::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* A byte-offset within another region, for handling pointer arithmetic
   as a region.  */

class offset_region : public region
{
public:
  /* A support class for uniquifying instances of offset_region.  */
  struct key_t
  {
    key_t (const region *parent, tree element_type, const svalue *byte_offset)
    : m_parent (parent), m_element_type (element_type), m_byte_offset (byte_offset)
    {
      gcc_assert (byte_offset);
    }

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_parent);
      hstate.add_ptr (m_element_type);
      hstate.add_ptr (m_byte_offset);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_parent == other.m_parent
	      && m_element_type == other.m_element_type
	      && m_byte_offset == other.m_byte_offset);
    }

    void mark_deleted () { m_byte_offset = reinterpret_cast<const svalue *> (1); }
    void mark_empty () { m_byte_offset = NULL; }
    bool is_deleted () const
    {
      return m_byte_offset == reinterpret_cast<const svalue *> (1);
    }
    bool is_empty () const { return m_byte_offset == NULL; }

    const region *m_parent;
    tree m_element_type;
    const svalue *m_byte_offset;
  };

  offset_region (symbol::id_t id, const region *parent, tree type,
		 const svalue *byte_offset)
  : region (complexity::from_pair (parent, byte_offset), id, parent, type),
    m_byte_offset (byte_offset)
  {}

  enum region_kind get_kind () const final override { return RK_OFFSET; }
  const offset_region *
  dyn_cast_offset_region () const final override { return this; }

  void accept (visitor *v) const final override;

  void dump_to_pp (pretty_printer *pp, bool simple) const final override;

  const svalue *get_byte_offset () const { return m_byte_offset; }
  const svalue *get_bit_offset (region_model_manager *mgr) const;

  bool get_relative_concrete_offset (bit_offset_t *out) const final override;
  const svalue *get_relative_symbolic_offset (region_model_manager *mgr)
    const final override;
  const svalue * get_byte_size_sval (region_model_manager *mgr)
    const final override;
  const svalue * get_bit_size_sval (region_model_manager *mgr)
    const final override;

private:
  const svalue *m_byte_offset;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const offset_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_OFFSET;
}

template <> struct default_hash_traits<offset_region::key_t>
: public member_function_hash_traits<offset_region::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* A region that is size BYTES_SIZE_SVAL in size within its parent
   region (or possibly larger, which would lead to an overflow.  */

class sized_region : public region
{
public:
  /* A support class for uniquifying instances of sized_region.  */
  struct key_t
  {
    key_t (const region *parent, tree element_type,
	   const svalue *byte_size_sval)
      : m_parent (parent), m_element_type (element_type),
	m_byte_size_sval (byte_size_sval)
    {
      gcc_assert (byte_size_sval);
    }

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_parent);
      hstate.add_ptr (m_element_type);
      hstate.add_ptr (m_byte_size_sval);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_parent == other.m_parent
	      && m_element_type == other.m_element_type
	      && m_byte_size_sval == other.m_byte_size_sval);
    }

    void mark_deleted () { m_byte_size_sval = reinterpret_cast<const svalue *> (1); }
    void mark_empty () { m_byte_size_sval = NULL; }
    bool is_deleted () const
    {
      return m_byte_size_sval == reinterpret_cast<const svalue *> (1);
    }
    bool is_empty () const { return m_byte_size_sval == NULL; }

    const region *m_parent;
    tree m_element_type;
    const svalue *m_byte_size_sval;
    const svalue *m_end_offset;
  };

  sized_region (symbol::id_t id, const region *parent, tree type,
		const svalue *byte_size_sval)
  : region (complexity::from_pair (parent, byte_size_sval),
	    id, parent, type),
    m_byte_size_sval (byte_size_sval)
  {}

  enum region_kind get_kind () const final override { return RK_SIZED; }
  const sized_region *
  dyn_cast_sized_region () const final override { return this; }

  void accept (visitor *v) const final override;

  void dump_to_pp (pretty_printer *pp, bool simple) const final override;

  bool get_byte_size (byte_size_t *out) const final override;
  bool get_bit_size (bit_size_t *out) const final override;

  const svalue *
  get_byte_size_sval (region_model_manager *) const final override
  {
    return m_byte_size_sval;
  }

  const svalue *
  get_bit_size_sval (region_model_manager *) const final override;

private:
  const svalue *m_byte_size_sval;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const sized_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_SIZED;
}

template <> struct default_hash_traits<sized_region::key_t>
: public member_function_hash_traits<sized_region::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* A region that views another region using a different type.  */

class cast_region : public region
{
public:
  /* A support class for uniquifying instances of cast_region.  */
  struct key_t
  {
    key_t (const region *original_region, tree type)
    : m_original_region (original_region), m_type (type)
    {
      gcc_assert (original_region);
    }

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_original_region);
      hstate.add_ptr (m_type);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_original_region == other.m_original_region
	      && m_type == other.m_type);
    }

    void mark_deleted ()
    {
      m_original_region = reinterpret_cast<const region *> (1);
    }
    void mark_empty () { m_original_region = nullptr; }
    bool is_deleted () const
    {
      return m_original_region == reinterpret_cast<const region *> (1);
    }
    bool is_empty () const { return m_original_region == nullptr; }

    const region *m_original_region;
    tree m_type;
  };

  cast_region (symbol::id_t id, const region *original_region, tree type)
  : region (complexity (original_region), id,
	    original_region->get_parent_region (), type),
    m_original_region (original_region)
  {}

  enum region_kind get_kind () const final override { return RK_CAST; }
  const cast_region *
  dyn_cast_cast_region () const final override { return this; }
  void accept (visitor *v) const final override;
  void dump_to_pp (pretty_printer *pp, bool simple) const final override;

  bool get_relative_concrete_offset (bit_offset_t *out) const final override;

  const region *get_original_region () const { return m_original_region; }

private:
  const region *m_original_region;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const cast_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_CAST;
}

template <> struct default_hash_traits<cast_region::key_t>
: public member_function_hash_traits<cast_region::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* An untyped region dynamically allocated on the heap via "malloc"
   or similar.  */

class heap_allocated_region : public region
{
public:
  heap_allocated_region (symbol::id_t id, const region *parent)
  : region (complexity (parent), id, parent, NULL_TREE)
  {}

  enum region_kind
  get_kind () const final override { return RK_HEAP_ALLOCATED; }

  void dump_to_pp (pretty_printer *pp, bool simple) const final override;
};

/* An untyped region dynamically allocated on the stack via "alloca".  */

class alloca_region : public region
{
public:
  alloca_region (symbol::id_t id, const frame_region *parent)
  : region (complexity (parent), id, parent, NULL_TREE)
  {}

  enum region_kind get_kind () const final override { return RK_ALLOCA; }

  void dump_to_pp (pretty_printer *pp, bool simple) const final override;
};

/* A region for a STRING_CST.  */

class string_region : public region
{
public:
  string_region (symbol::id_t id, const region *parent, tree string_cst)
  : region (complexity (parent), id, parent, TREE_TYPE (string_cst)),
    m_string_cst (string_cst)
  {}

  const string_region *
  dyn_cast_string_region () const final override { return this; }

  enum region_kind get_kind () const final override { return RK_STRING; }

  void dump_to_pp (pretty_printer *pp, bool simple) const final override;

  /* We assume string literals are immutable, so we don't track them in
     the store.  */
  bool tracked_p () const final override { return false; }

  tree get_string_cst () const { return m_string_cst; }

private:
  tree m_string_cst;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const string_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_STRING;
}

namespace ana {

/* A region for a specific range of bits within another region.  */

class bit_range_region : public region
{
public:
  /* A support class for uniquifying instances of bit_range_region.  */
  struct key_t
  {
    key_t (const region *parent, tree type, const bit_range &bits)
    : m_parent (parent), m_type (type), m_bits (bits)
    {
      gcc_assert (parent);
    }

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_parent);
      hstate.add_ptr (m_type);
      hstate.add_wide_int (m_bits.m_start_bit_offset);
      hstate.add_wide_int (m_bits.m_size_in_bits);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_parent == other.m_parent
	      && m_type == other.m_type
	      && m_bits == other.m_bits);
    }

    void mark_deleted () { m_parent = reinterpret_cast<const region *> (1); }
    void mark_empty () { m_parent = NULL; }
    bool is_deleted () const
    {
      return m_parent == reinterpret_cast<const region *> (1);
    }
    bool is_empty () const { return m_parent == NULL; }

    const region *m_parent;
    tree m_type;
    bit_range m_bits;
  };

  bit_range_region (symbol::id_t id, const region *parent, tree type,
		    const bit_range &bits)
  : region (complexity (parent), id, parent, type),
    m_bits (bits)
  {}

  const bit_range_region *
  dyn_cast_bit_range_region () const final override { return this; }

  enum region_kind get_kind () const final override { return RK_BIT_RANGE; }

  void dump_to_pp (pretty_printer *pp, bool simple) const final override;

  const bit_range &get_bits () const { return m_bits; }

  bool get_byte_size (byte_size_t *out) const final override;
  bool get_bit_size (bit_size_t *out) const final override;
  const svalue *get_byte_size_sval (region_model_manager *mgr) const final override;
  const svalue *get_bit_size_sval (region_model_manager *mgr) const final override;
  bool get_relative_concrete_offset (bit_offset_t *out) const final override;
  const svalue *get_relative_symbolic_offset (region_model_manager *mgr)
    const final override;

private:
  bit_range m_bits;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const bit_range_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_BIT_RANGE;
}

template <> struct default_hash_traits<bit_range_region::key_t>
: public member_function_hash_traits<bit_range_region::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* A region for the N-th vararg within a frame_region for a variadic call.  */

class var_arg_region : public region
{
public:
  /* A support class for uniquifying instances of var_arg_region.  */
  struct key_t
  {
    key_t (const frame_region *parent, unsigned idx)
    : m_parent (parent), m_idx (idx)
    {
      gcc_assert (parent);
    }

    hashval_t hash () const
    {
      inchash::hash hstate;
      hstate.add_ptr (m_parent);
      hstate.add_int (m_idx);
      return hstate.end ();
    }

    bool operator== (const key_t &other) const
    {
      return (m_parent == other.m_parent
	      && m_idx == other.m_idx);
    }

    void mark_deleted ()
    {
      m_parent = reinterpret_cast<const frame_region *> (1);
    }
    void mark_empty () { m_parent = NULL; }
    bool is_deleted () const
    {
      return m_parent == reinterpret_cast<const frame_region *> (1);
    }
    bool is_empty () const { return m_parent == NULL; }

    const frame_region *m_parent;
    unsigned m_idx;
  };

  var_arg_region (symbol::id_t id, const frame_region *parent,
		  unsigned idx)
  : region (complexity (parent), id, parent, NULL_TREE),
    m_idx (idx)
  {}

  const var_arg_region *
  dyn_cast_var_arg_region () const final override { return this; }

  enum region_kind get_kind () const final override { return RK_VAR_ARG; }

  void dump_to_pp (pretty_printer *pp, bool simple) const final override;

  const frame_region *get_frame_region () const;
  unsigned get_index () const { return m_idx; }

private:
  unsigned m_idx;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const var_arg_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_VAR_ARG;
}

template <> struct default_hash_traits<var_arg_region::key_t>
: public member_function_hash_traits<var_arg_region::key_t>
{
  static const bool empty_zero_p = true;
};

namespace ana {

/* A region for errno for the current thread.  */

class errno_region : public region
{
public:
  errno_region (symbol::id_t id, const thread_local_region *parent)
  : region (complexity (parent), id, parent, integer_type_node)
  {}

  enum region_kind get_kind () const final override { return RK_ERRNO; }

  void dump_to_pp (pretty_printer *pp, bool simple) const final override;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const errno_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_ERRNO;
}

namespace ana {

/* Similar to a decl region, but we don't have the decl.
   For implementing e.g. static buffers of known_functions,
   or other internal state of an API.

   These are owned by known_function instances, rather than the
   region_model_manager.  */

class private_region : public region
{
public:
  private_region (unsigned id, const region *parent, tree type,
		  const char *desc)
  : region (complexity (parent), id, parent, type),
    m_desc (desc)
  {}

  enum region_kind get_kind () const final override { return RK_PRIVATE; }

  void dump_to_pp (pretty_printer *pp, bool simple) const final override;

private:
  const char *m_desc;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const private_region *>::test (const region *reg)
{
  return reg->get_kind () == RK_PRIVATE;
}

namespace ana {

/* An unknown region, for handling unimplemented tree codes.  */

class unknown_region : public region
{
public:
  unknown_region (symbol::id_t id, const region *parent, tree type)
  : region (complexity (parent), id, parent, type)
  {}

  enum region_kind get_kind () const final override { return RK_UNKNOWN; }

  void dump_to_pp (pretty_printer *pp, bool simple) const final override;
};

} // namespace ana

#endif /* GCC_ANALYZER_REGION_H */
