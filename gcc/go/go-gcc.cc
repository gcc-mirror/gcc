// go-gcc.cc -- Go frontend to gcc IR.
// Copyright (C) 2011 Free Software Foundation, Inc.
// Contributed by Ian Lance Taylor, Google.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "go-system.h"

// This has to be included outside of extern "C", so we have to
// include it here before tree.h includes it later.
#include <gmp.h>

#ifndef ENABLE_BUILD_WITH_CXX
extern "C"
{
#endif

#include "tree.h"
#include "tree-iterator.h"
#include "gimple.h"

#ifndef ENABLE_BUILD_WITH_CXX
}
#endif

#include "gogo.h"
#include "backend.h"

// A class wrapping a tree.

class Gcc_tree
{
 public:
  Gcc_tree(tree t)
    : t_(t)
  { }

  tree
  get_tree()
  { return this->t_; }

 private:
  tree t_;
};

// In gcc, types, expressions, and statements are all trees.
class Btype : public Gcc_tree
{
 public:
  Btype(tree t)
    : Gcc_tree(t)
  { }
};

class Bexpression : public Gcc_tree
{
 public:
  Bexpression(tree t)
    : Gcc_tree(t)
  { }
};

class Bstatement : public Gcc_tree
{
 public:
  Bstatement(tree t)
    : Gcc_tree(t)
  { }
};

class Bfunction : public Gcc_tree
{
 public:
  Bfunction(tree t)
    : Gcc_tree(t)
  { }
};

class Blabel : public Gcc_tree
{
 public:
  Blabel(tree t)
    : Gcc_tree(t)
  { }
};

// This file implements the interface between the Go frontend proper
// and the gcc IR.  This implements specific instantiations of
// abstract classes defined by the Go frontend proper.  The Go
// frontend proper class methods of these classes to generate the
// backend representation.

class Gcc_backend : public Backend
{
 public:
  // Types.

  Btype*
  error_type()
  { gcc_unreachable(); }

  Btype*
  void_type()
  { gcc_unreachable(); }

  Btype*
  bool_type()
  { gcc_unreachable(); }

  Btype*
  integer_type(bool /* is_unsigned */, int /* bits */)
  { gcc_unreachable(); }

  Btype*
  float_type(int /* bits */)
  { gcc_unreachable(); }

  Btype*
  string_type()
  { gcc_unreachable(); }

  Btype*
  function_type(const Function_type*, Btype* /* receiver */,
		const Btypes* /* parameters */,
		const Btypes* /* results */)
  { gcc_unreachable(); }

  Btype*
  struct_type(const Struct_type*, const Btypes* /* field_types */)
  { gcc_unreachable(); }

  Btype*
  array_type(const Btype* /* element_type */, const Bexpression* /* length */)
  { gcc_unreachable(); }

  Btype*
  slice_type(const Btype* /* element_type */)
  { gcc_unreachable(); }

  Btype*
  map_type(const Btype* /* key_type */, const Btype* /* value_type */,
	   source_location)
  { gcc_unreachable(); }

  Btype*
  channel_type(const Btype* /* element_type */)
  { gcc_unreachable(); }

  Btype*
  interface_type(const Interface_type*, const Btypes* /* method_types */)
  { gcc_unreachable(); }

  // Statements.

  Bstatement*
  expression_statement(Bexpression*);

  Bstatement*
  assignment_statement(Bexpression* lhs, Bexpression* rhs, source_location);

  Bstatement*
  return_statement(Bfunction*, const std::vector<Bexpression*>&,
		   source_location);

  Bstatement*
  if_statement(Bexpression* condition, Bstatement* then_block,
	       Bstatement* else_block, source_location);

  Bstatement*
  switch_statement(Bexpression* value,
		   const std::vector<std::vector<Bexpression*> >& cases,
		   const std::vector<Bstatement*>& statements,
		   source_location);

  Bstatement*
  statement_list(const std::vector<Bstatement*>&);

  // Labels.

  Blabel*
  label(Bfunction*, const std::string& name, source_location);

  Bstatement*
  label_definition_statement(Blabel*);

  Bstatement*
  goto_statement(Blabel*, source_location);

  Bexpression*
  label_address(Blabel*, source_location);

 private:
  // Make a Bexpression from a tree.
  Bexpression*
  make_expression(tree t)
  { return new Bexpression(t); }

  // Make a Bstatement from a tree.
  Bstatement*
  make_statement(tree t)
  { return new Bstatement(t); }
};

// A helper function.

static inline tree
get_identifier_from_string(const std::string& str)
{
  return get_identifier_with_length(str.data(), str.length());
}

// An expression as a statement.

Bstatement*
Gcc_backend::expression_statement(Bexpression* expr)
{
  return this->make_statement(expr->get_tree());
}

// Assignment.

Bstatement*
Gcc_backend::assignment_statement(Bexpression* lhs, Bexpression* rhs,
				  source_location location)
{
  tree lhs_tree = lhs->get_tree();
  tree rhs_tree = rhs->get_tree();
  if (lhs_tree == error_mark_node || rhs_tree == error_mark_node)
    return this->make_statement(error_mark_node);
  return this->make_statement(fold_build2_loc(location, MODIFY_EXPR,
					      void_type_node,
					      lhs_tree, rhs_tree));
}

// Return.

Bstatement*
Gcc_backend::return_statement(Bfunction* bfunction,
			      const std::vector<Bexpression*>& vals,
			      source_location location)
{
  tree fntree = bfunction->get_tree();
  if (fntree == error_mark_node)
    return this->make_statement(error_mark_node);
  tree result = DECL_RESULT(fntree);
  if (result == error_mark_node)
    return this->make_statement(error_mark_node);
  tree ret;
  if (vals.empty())
    ret = fold_build1_loc(location, RETURN_EXPR, void_type_node, NULL_TREE);
  else if (vals.size() == 1)
    {
      tree val = vals.front()->get_tree();
      if (val == error_mark_node)
	return this->make_statement(error_mark_node);
      tree set = fold_build2_loc(location, MODIFY_EXPR, void_type_node,
				 result, vals.front()->get_tree());
      ret = fold_build1_loc(location, RETURN_EXPR, void_type_node, set);
    }
  else
    {
      // To return multiple values, copy the values into a temporary
      // variable of the right structure type, and then assign the
      // temporary variable to the DECL_RESULT in the return
      // statement.
      tree stmt_list = NULL_TREE;
      tree rettype = TREE_TYPE(result);
      tree rettmp = create_tmp_var(rettype, "RESULT");
      tree field = TYPE_FIELDS(rettype);
      for (std::vector<Bexpression*>::const_iterator p = vals.begin();
	   p != vals.end();
	   p++, field = DECL_CHAIN(field))
	{
	  gcc_assert(field != NULL_TREE);
	  tree ref = fold_build3_loc(location, COMPONENT_REF, TREE_TYPE(field),
				     rettmp, field, NULL_TREE);
	  tree val = (*p)->get_tree();
	  if (val == error_mark_node)
	    return this->make_statement(error_mark_node);
	  tree set = fold_build2_loc(location, MODIFY_EXPR, void_type_node,
				     ref, (*p)->get_tree());
	  append_to_statement_list(set, &stmt_list);
	}
      gcc_assert(field == NULL_TREE);
      tree set = fold_build2_loc(location, MODIFY_EXPR, void_type_node,
				 result, rettmp);
      tree ret_expr = fold_build1_loc(location, RETURN_EXPR, void_type_node,
				      set);
      append_to_statement_list(ret_expr, &stmt_list);
      ret = stmt_list;
    }
  return this->make_statement(ret);
}

// If.

Bstatement*
Gcc_backend::if_statement(Bexpression* condition, Bstatement* then_block,
			  Bstatement* else_block, source_location location)
{
  tree cond_tree = condition->get_tree();
  tree then_tree = then_block->get_tree();
  tree else_tree = else_block == NULL ? NULL_TREE : else_block->get_tree();
  if (cond_tree == error_mark_node
      || then_tree == error_mark_node
      || else_tree == error_mark_node)
    return this->make_statement(error_mark_node);
  tree ret = build3_loc(location, COND_EXPR, void_type_node, cond_tree,
			then_tree, else_tree);
  return this->make_statement(ret);
}

// Switch.

Bstatement*
Gcc_backend::switch_statement(
    Bexpression* value,
    const std::vector<std::vector<Bexpression*> >& cases,
    const std::vector<Bstatement*>& statements,
    source_location switch_location)
{
  gcc_assert(cases.size() == statements.size());

  tree stmt_list = NULL_TREE;
  std::vector<std::vector<Bexpression*> >::const_iterator pc = cases.begin();
  for (std::vector<Bstatement*>::const_iterator ps = statements.begin();
       ps != statements.end();
       ++ps, ++pc)
    {
      if (pc->empty())
	{
	  source_location loc = (*ps != NULL
				 ? EXPR_LOCATION((*ps)->get_tree())
				 : UNKNOWN_LOCATION);
	  tree label = create_artificial_label(loc);
	  tree c = build3_loc(loc, CASE_LABEL_EXPR, void_type_node, NULL_TREE,
			      NULL_TREE, label);
	  append_to_statement_list(c, &stmt_list);
	}
      else
	{
	  for (std::vector<Bexpression*>::const_iterator pcv = pc->begin();
	       pcv != pc->end();
	       ++pcv)
	    {
	      tree t = (*pcv)->get_tree();
	      if (t == error_mark_node)
		return this->make_statement(error_mark_node);
	      source_location loc = EXPR_LOCATION(t);
	      tree label = create_artificial_label(loc);
	      tree c = build3_loc(loc, CASE_LABEL_EXPR, void_type_node,
				  (*pcv)->get_tree(), NULL_TREE, label);
	      append_to_statement_list(c, &stmt_list);
	    }
	}

      if (*ps != NULL)
	{
	  tree t = (*ps)->get_tree();
	  if (t == error_mark_node)
	    return this->make_statement(error_mark_node);
	  append_to_statement_list(t, &stmt_list);
	}
    }

  tree tv = value->get_tree();
  if (tv == error_mark_node)
    return this->make_statement(error_mark_node);
  tree t = build3_loc(switch_location, SWITCH_EXPR, void_type_node,
		      tv, stmt_list, NULL_TREE);
  return this->make_statement(t);
}

// List of statements.

Bstatement*
Gcc_backend::statement_list(const std::vector<Bstatement*>& statements)
{
  tree stmt_list = NULL_TREE;
  for (std::vector<Bstatement*>::const_iterator p = statements.begin();
       p != statements.end();
       ++p)
    {
      tree t = (*p)->get_tree();
      if (t == error_mark_node)
	return this->make_statement(error_mark_node);
      append_to_statement_list(t, &stmt_list);
    }
  return this->make_statement(stmt_list);
}

// Make a label.

Blabel*
Gcc_backend::label(Bfunction* function, const std::string& name,
		   source_location location)
{
  tree decl;
  if (name.empty())
    decl = create_artificial_label(location);
  else
    {
      tree id = get_identifier_from_string(name);
      decl = build_decl(location, LABEL_DECL, id, void_type_node);
      DECL_CONTEXT(decl) = function->get_tree();
    }
  return new Blabel(decl);
}

// Make a statement which defines a label.

Bstatement*
Gcc_backend::label_definition_statement(Blabel* label)
{
  tree lab = label->get_tree();
  tree ret = fold_build1_loc(DECL_SOURCE_LOCATION(lab), LABEL_EXPR,
			     void_type_node, lab);
  return this->make_statement(ret);
}

// Make a goto statement.

Bstatement*
Gcc_backend::goto_statement(Blabel* label, source_location location)
{
  tree lab = label->get_tree();
  tree ret = fold_build1_loc(location, GOTO_EXPR, void_type_node, lab);
  return this->make_statement(ret);
}

// Get the address of a label.

Bexpression*
Gcc_backend::label_address(Blabel* label, source_location location)
{
  tree lab = label->get_tree();
  TREE_USED(lab) = 1;
  TREE_ADDRESSABLE(lab) = 1;
  tree ret = fold_convert_loc(location, ptr_type_node,
			      build_fold_addr_expr_loc(location, lab));
  return this->make_expression(ret);
}

// The single backend.

static Gcc_backend gcc_backend;

// Return the backend generator.

Backend*
go_get_backend()
{
  return &gcc_backend;
}

// FIXME: Temporary functions while converting to the new backend
// interface.

Bexpression*
tree_to_expr(tree t)
{
  return new Bexpression(t);
}

Bstatement*
tree_to_stat(tree t)
{
  return new Bstatement(t);
}

Bfunction*
tree_to_function(tree t)
{
  return new Bfunction(t);
}

tree
expr_to_tree(Bexpression* be)
{
  return be->get_tree();
}

tree
stat_to_tree(Bstatement* bs)
{
  return bs->get_tree();
}
