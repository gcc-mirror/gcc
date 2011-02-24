// types.cc -- Go frontend types.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include <gmp.h>

#ifndef ENABLE_BUILD_WITH_CXX
extern "C"
{
#endif

#include "toplev.h"
#include "intl.h"
#include "tree.h"
#include "gimple.h"
#include "real.h"
#include "convert.h"

#ifndef ENABLE_BUILD_WITH_CXX
}
#endif

#include "go-c.h"
#include "gogo.h"
#include "operator.h"
#include "expressions.h"
#include "statements.h"
#include "export.h"
#include "import.h"
#include "types.h"

// Class Type.

Type::Type(Type_classification classification)
  : classification_(classification), tree_(NULL_TREE),
    type_descriptor_decl_(NULL_TREE)
{
}

Type::~Type()
{
}

// Get the base type for a type--skip names and forward declarations.

Type*
Type::base()
{
  switch (this->classification_)
    {
    case TYPE_NAMED:
      return this->named_type()->named_base();
    case TYPE_FORWARD:
      return this->forward_declaration_type()->real_type()->base();
    default:
      return this;
    }
}

const Type*
Type::base() const
{
  switch (this->classification_)
    {
    case TYPE_NAMED:
      return this->named_type()->named_base();
    case TYPE_FORWARD:
      return this->forward_declaration_type()->real_type()->base();
    default:
      return this;
    }
}

// Skip defined forward declarations.

Type*
Type::forwarded()
{
  Type* t = this;
  Forward_declaration_type* ftype = t->forward_declaration_type();
  while (ftype != NULL && ftype->is_defined())
    {
      t = ftype->real_type();
      ftype = t->forward_declaration_type();
    }
  return t;
}

const Type*
Type::forwarded() const
{
  const Type* t = this;
  const Forward_declaration_type* ftype = t->forward_declaration_type();
  while (ftype != NULL && ftype->is_defined())
    {
      t = ftype->real_type();
      ftype = t->forward_declaration_type();
    }
  return t;
}

// If this is a named type, return it.  Otherwise, return NULL.

Named_type*
Type::named_type()
{
  return this->forwarded()->convert_no_base<Named_type, TYPE_NAMED>();
}

const Named_type*
Type::named_type() const
{
  return this->forwarded()->convert_no_base<const Named_type, TYPE_NAMED>();
}

// Return true if this type is not defined.

bool
Type::is_undefined() const
{
  return this->forwarded()->forward_declaration_type() != NULL;
}

// Return true if this is a basic type: a type which is not composed
// of other types, and is not void.

bool
Type::is_basic_type() const
{
  switch (this->classification_)
    {
    case TYPE_INTEGER:
    case TYPE_FLOAT:
    case TYPE_COMPLEX:
    case TYPE_BOOLEAN:
    case TYPE_STRING:
    case TYPE_NIL:
      return true;

    case TYPE_ERROR:
    case TYPE_VOID:
    case TYPE_FUNCTION:
    case TYPE_POINTER:
    case TYPE_STRUCT:
    case TYPE_ARRAY:
    case TYPE_MAP:
    case TYPE_CHANNEL:
    case TYPE_INTERFACE:
      return false;

    case TYPE_NAMED:
    case TYPE_FORWARD:
      return this->base()->is_basic_type();

    default:
      gcc_unreachable();
    }
}

// Return true if this is an abstract type.

bool
Type::is_abstract() const
{
  switch (this->classification())
    {
    case TYPE_INTEGER:
      return this->integer_type()->is_abstract();
    case TYPE_FLOAT:
      return this->float_type()->is_abstract();
    case TYPE_COMPLEX:
      return this->complex_type()->is_abstract();
    case TYPE_STRING:
      return this->is_abstract_string_type();
    case TYPE_BOOLEAN:
      return this->is_abstract_boolean_type();
    default:
      return false;
    }
}

// Return a non-abstract version of an abstract type.

Type*
Type::make_non_abstract_type()
{
  gcc_assert(this->is_abstract());
  switch (this->classification())
    {
    case TYPE_INTEGER:
      return Type::lookup_integer_type("int");
    case TYPE_FLOAT:
      return Type::lookup_float_type("float64");
    case TYPE_COMPLEX:
      return Type::lookup_complex_type("complex128");
    case TYPE_STRING:
      return Type::lookup_string_type();
    case TYPE_BOOLEAN:
      return Type::lookup_bool_type();
    default:
      gcc_unreachable();
    }
}

// Return true if this is an error type.  Don't give an error if we
// try to dereference an undefined forwarding type, as this is called
// in the parser when the type may legitimately be undefined.

bool
Type::is_error_type() const
{
  const Type* t = this->forwarded();
  // Note that we return false for an undefined forward type.
  switch (t->classification_)
    {
    case TYPE_ERROR:
      return true;
    case TYPE_NAMED:
      return t->named_type()->is_named_error_type();
    default:
      return false;
    }
}

// If this is a pointer type, return the type to which it points.
// Otherwise, return NULL.

Type*
Type::points_to() const
{
  const Pointer_type* ptype = this->convert<const Pointer_type,
					    TYPE_POINTER>();
  return ptype == NULL ? NULL : ptype->points_to();
}

// Return whether this is an open array type.

bool
Type::is_open_array_type() const
{
  return this->array_type() != NULL && this->array_type()->length() == NULL;
}

// Return whether this is the predeclared constant nil being used as a
// type.

bool
Type::is_nil_constant_as_type() const
{
  const Type* t = this->forwarded();
  if (t->forward_declaration_type() != NULL)
    {
      const Named_object* no = t->forward_declaration_type()->named_object();
      if (no->is_unknown())
	no = no->unknown_value()->real_named_object();
      if (no != NULL
	  && no->is_const()
	  && no->const_value()->expr()->is_nil_expression())
	return true;
    }
  return false;
}

// Traverse a type.

int
Type::traverse(Type* type, Traverse* traverse)
{
  gcc_assert((traverse->traverse_mask() & Traverse::traverse_types) != 0
	     || (traverse->traverse_mask()
		 & Traverse::traverse_expressions) != 0);
  if (traverse->remember_type(type))
    {
      // We have already traversed this type.
      return TRAVERSE_CONTINUE;
    }
  if ((traverse->traverse_mask() & Traverse::traverse_types) != 0)
    {
      int t = traverse->type(type);
      if (t == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
      else if (t == TRAVERSE_SKIP_COMPONENTS)
	return TRAVERSE_CONTINUE;
    }
  // An array type has an expression which we need to traverse if
  // traverse_expressions is set.
  if (type->do_traverse(traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Default implementation for do_traverse for child class.

int
Type::do_traverse(Traverse*)
{
  return TRAVERSE_CONTINUE;
}

// Return whether two types are identical.  If ERRORS_ARE_IDENTICAL,
// then return true for all erroneous types; this is used to avoid
// cascading errors.  If REASON is not NULL, optionally set *REASON to
// the reason the types are not identical.

bool
Type::are_identical(const Type* t1, const Type* t2, bool errors_are_identical,
		    std::string* reason)
{
  if (t1 == NULL || t2 == NULL)
    {
      // Something is wrong.
      return errors_are_identical ? true : t1 == t2;
    }

  // Skip defined forward declarations.
  t1 = t1->forwarded();
  t2 = t2->forwarded();

  if (t1 == t2)
    return true;

  // An undefined forward declaration is an error.
  if (t1->forward_declaration_type() != NULL
      || t2->forward_declaration_type() != NULL)
    return errors_are_identical;

  // Avoid cascading errors with error types.
  if (t1->is_error_type() || t2->is_error_type())
    {
      if (errors_are_identical)
	return true;
      return t1->is_error_type() && t2->is_error_type();
    }

  // Get a good reason for the sink type.  Note that the sink type on
  // the left hand side of an assignment is handled in are_assignable.
  if (t1->is_sink_type() || t2->is_sink_type())
    {
      if (reason != NULL)
	*reason = "invalid use of _";
      return false;
    }

  // A named type is only identical to itself.
  if (t1->named_type() != NULL || t2->named_type() != NULL)
    return false;

  // Check type shapes.
  if (t1->classification() != t2->classification())
    return false;

  switch (t1->classification())
    {
    case TYPE_VOID:
    case TYPE_BOOLEAN:
    case TYPE_STRING:
    case TYPE_NIL:
      // These types are always identical.
      return true;

    case TYPE_INTEGER:
      return t1->integer_type()->is_identical(t2->integer_type());

    case TYPE_FLOAT:
      return t1->float_type()->is_identical(t2->float_type());

    case TYPE_COMPLEX:
      return t1->complex_type()->is_identical(t2->complex_type());

    case TYPE_FUNCTION:
      return t1->function_type()->is_identical(t2->function_type(),
					       false,
					       errors_are_identical,
					       reason);

    case TYPE_POINTER:
      return Type::are_identical(t1->points_to(), t2->points_to(),
				 errors_are_identical, reason);

    case TYPE_STRUCT:
      return t1->struct_type()->is_identical(t2->struct_type(),
					     errors_are_identical);

    case TYPE_ARRAY:
      return t1->array_type()->is_identical(t2->array_type(),
					    errors_are_identical);

    case TYPE_MAP:
      return t1->map_type()->is_identical(t2->map_type(),
					  errors_are_identical);

    case TYPE_CHANNEL:
      return t1->channel_type()->is_identical(t2->channel_type(),
					      errors_are_identical);

    case TYPE_INTERFACE:
      return t1->interface_type()->is_identical(t2->interface_type(),
						errors_are_identical);

    case TYPE_CALL_MULTIPLE_RESULT:
      if (reason != NULL)
	*reason = "invalid use of multiple value function call";
      return false;

    default:
      gcc_unreachable();
    }
}

// Return true if it's OK to have a binary operation with types LHS
// and RHS.  This is not used for shifts or comparisons.

bool
Type::are_compatible_for_binop(const Type* lhs, const Type* rhs)
{
  if (Type::are_identical(lhs, rhs, true, NULL))
    return true;

  // A constant of abstract bool type may be mixed with any bool type.
  if ((rhs->is_abstract_boolean_type() && lhs->is_boolean_type())
      || (lhs->is_abstract_boolean_type() && rhs->is_boolean_type()))
    return true;

  // A constant of abstract string type may be mixed with any string
  // type.
  if ((rhs->is_abstract_string_type() && lhs->is_string_type())
      || (lhs->is_abstract_string_type() && rhs->is_string_type()))
    return true;

  lhs = lhs->base();
  rhs = rhs->base();

  // A constant of abstract integer, float, or complex type may be
  // mixed with an integer, float, or complex type.
  if ((rhs->is_abstract()
       && (rhs->integer_type() != NULL
	   || rhs->float_type() != NULL
	   || rhs->complex_type() != NULL)
       && (lhs->integer_type() != NULL
	   || lhs->float_type() != NULL
	   || lhs->complex_type() != NULL))
      || (lhs->is_abstract()
	  && (lhs->integer_type() != NULL
	      || lhs->float_type() != NULL
	      || lhs->complex_type() != NULL)
	  && (rhs->integer_type() != NULL
	      || rhs->float_type() != NULL
	      || rhs->complex_type() != NULL)))
    return true;

  // The nil type may be compared to a pointer, an interface type, a
  // slice type, a channel type, a map type, or a function type.
  if (lhs->is_nil_type()
      && (rhs->points_to() != NULL
	  || rhs->interface_type() != NULL
	  || rhs->is_open_array_type()
	  || rhs->map_type() != NULL
	  || rhs->channel_type() != NULL
	  || rhs->function_type() != NULL))
    return true;
  if (rhs->is_nil_type()
      && (lhs->points_to() != NULL
	  || lhs->interface_type() != NULL
	  || lhs->is_open_array_type()
	  || lhs->map_type() != NULL
	  || lhs->channel_type() != NULL
	  || lhs->function_type() != NULL))
    return true;

  return false;
}

// Return true if a value with type RHS may be assigned to a variable
// with type LHS.  If REASON is not NULL, set *REASON to the reason
// the types are not assignable.

bool
Type::are_assignable(const Type* lhs, const Type* rhs, std::string* reason)
{
  // Do some checks first.  Make sure the types are defined.
  if (rhs != NULL
      && rhs->forwarded()->forward_declaration_type() == NULL
      && rhs->is_void_type())
    {
      if (reason != NULL)
	*reason = "non-value used as value";
      return false;
    }

  if (lhs != NULL && lhs->forwarded()->forward_declaration_type() == NULL)
    {
      // Any value may be assigned to the blank identifier.
      if (lhs->is_sink_type())
	return true;

      // All fields of a struct must be exported, or the assignment
      // must be in the same package.
      if (rhs != NULL && rhs->forwarded()->forward_declaration_type() == NULL)
	{
	  if (lhs->has_hidden_fields(NULL, reason)
	      || rhs->has_hidden_fields(NULL, reason))
	    return false;
	}
    }

  // Identical types are assignable.
  if (Type::are_identical(lhs, rhs, true, reason))
    return true;

  // The types are assignable if they have identical underlying types
  // and either LHS or RHS is not a named type.
  if (((lhs->named_type() != NULL && rhs->named_type() == NULL)
       || (rhs->named_type() != NULL && lhs->named_type() == NULL))
      && Type::are_identical(lhs->base(), rhs->base(), true, reason))
    return true;

  // The types are assignable if LHS is an interface type and RHS
  // implements the required methods.
  const Interface_type* lhs_interface_type = lhs->interface_type();
  if (lhs_interface_type != NULL)
    {
      if (lhs_interface_type->implements_interface(rhs, reason))
	return true;
      const Interface_type* rhs_interface_type = rhs->interface_type();
      if (rhs_interface_type != NULL
	  && lhs_interface_type->is_compatible_for_assign(rhs_interface_type,
							  reason))
	return true;
    }

  // The type are assignable if RHS is a bidirectional channel type,
  // LHS is a channel type, they have identical element types, and
  // either LHS or RHS is not a named type.
  if (lhs->channel_type() != NULL
      && rhs->channel_type() != NULL
      && rhs->channel_type()->may_send()
      && rhs->channel_type()->may_receive()
      && (lhs->named_type() == NULL || rhs->named_type() == NULL)
      && Type::are_identical(lhs->channel_type()->element_type(),
			     rhs->channel_type()->element_type(),
			     true,
			     reason))
    return true;

  // The nil type may be assigned to a pointer, function, slice, map,
  // channel, or interface type.
  if (rhs->is_nil_type()
      && (lhs->points_to() != NULL
	  || lhs->function_type() != NULL
	  || lhs->is_open_array_type()
	  || lhs->map_type() != NULL
	  || lhs->channel_type() != NULL
	  || lhs->interface_type() != NULL))
    return true;

  // An untyped numeric constant may be assigned to a numeric type if
  // it is representable in that type.
  if ((rhs->is_abstract()
       && (rhs->integer_type() != NULL
	   || rhs->float_type() != NULL
	   || rhs->complex_type() != NULL))
      && (lhs->integer_type() != NULL
	  || lhs->float_type() != NULL
	  || lhs->complex_type() != NULL))
    return true;

  // Give some better error messages.
  if (reason != NULL && reason->empty())
    {
      if (rhs->interface_type() != NULL)
	reason->assign(_("need explicit conversion"));
      else if (rhs->is_call_multiple_result_type())
	reason->assign(_("multiple value function call in "
			 "single value context"));
      else if (lhs->named_type() != NULL && rhs->named_type() != NULL)
	{
	  size_t len = (lhs->named_type()->name().length()
			+ rhs->named_type()->name().length()
			+ 100);
	  char* buf = new char[len];
	  snprintf(buf, len, _("cannot use type %s as type %s"),
		   rhs->named_type()->message_name().c_str(),
		   lhs->named_type()->message_name().c_str());
	  reason->assign(buf);
	  delete[] buf;
	}
    }

  return false;
}

// Return true if a value with type RHS may be converted to type LHS.
// If REASON is not NULL, set *REASON to the reason the types are not
// convertible.

bool
Type::are_convertible(const Type* lhs, const Type* rhs, std::string* reason)
{
  // The types are convertible if they are assignable.
  if (Type::are_assignable(lhs, rhs, reason))
    return true;

  // The types are convertible if they have identical underlying
  // types.
  if ((lhs->named_type() != NULL || rhs->named_type() != NULL)
      && Type::are_identical(lhs->base(), rhs->base(), true, reason))
    return true;

  // The types are convertible if they are both unnamed pointer types
  // and their pointer base types have identical underlying types.
  if (lhs->named_type() == NULL
      && rhs->named_type() == NULL
      && lhs->points_to() != NULL
      && rhs->points_to() != NULL
      && (lhs->points_to()->named_type() != NULL
	  || rhs->points_to()->named_type() != NULL)
      && Type::are_identical(lhs->points_to()->base(),
			     rhs->points_to()->base(),
			     true,
			     reason))
    return true;

  // Integer and floating point types are convertible to each other.
  if ((lhs->integer_type() != NULL || lhs->float_type() != NULL)
      && (rhs->integer_type() != NULL || rhs->float_type() != NULL))
    return true;

  // Complex types are convertible to each other.
  if (lhs->complex_type() != NULL && rhs->complex_type() != NULL)
    return true;

  // An integer, or []byte, or []int, may be converted to a string.
  if (lhs->is_string_type())
    {
      if (rhs->integer_type() != NULL)
	return true;
      if (rhs->is_open_array_type() && rhs->named_type() == NULL)
	{
	  const Type* e = rhs->array_type()->element_type()->forwarded();
	  if (e->integer_type() != NULL
	      && (e == Type::lookup_integer_type("uint8")
		  || e == Type::lookup_integer_type("int")))
	    return true;
	}
    }

  // A string may be converted to []byte or []int.
  if (rhs->is_string_type()
      && lhs->is_open_array_type()
      && lhs->named_type() == NULL)
    {
      const Type* e = lhs->array_type()->element_type()->forwarded();
      if (e->integer_type() != NULL
	  && (e == Type::lookup_integer_type("uint8")
	      || e == Type::lookup_integer_type("int")))
	return true;
    }

  // An unsafe.Pointer type may be converted to any pointer type or to
  // uintptr, and vice-versa.
  if (lhs->is_unsafe_pointer_type()
      && (rhs->points_to() != NULL
	  || (rhs->integer_type() != NULL
	      && rhs->forwarded() == Type::lookup_integer_type("uintptr"))))
    return true;
  if (rhs->is_unsafe_pointer_type()
      && (lhs->points_to() != NULL
	  || (lhs->integer_type() != NULL
	      && lhs->forwarded() == Type::lookup_integer_type("uintptr"))))
    return true;

  // Give a better error message.
  if (reason != NULL)
    {
      if (reason->empty())
	*reason = "invalid type conversion";
      else
	{
	  std::string s = "invalid type conversion (";
	  s += *reason;
	  s += ')';
	  *reason = s;
	}
    }

  return false;
}

// Return whether this type has any hidden fields.  This is only a
// possibility for a few types.

bool
Type::has_hidden_fields(const Named_type* within, std::string* reason) const
{
  switch (this->forwarded()->classification_)
    {
    case TYPE_NAMED:
      return this->named_type()->named_type_has_hidden_fields(reason);
    case TYPE_STRUCT:
      return this->struct_type()->struct_has_hidden_fields(within, reason);
    case TYPE_ARRAY:
      return this->array_type()->array_has_hidden_fields(within, reason);
    default:
      return false;
    }
}

// Return a hash code for the type to be used for method lookup.

unsigned int
Type::hash_for_method(Gogo* gogo) const
{
  unsigned int ret = 0;
  if (this->classification_ != TYPE_FORWARD)
    ret += this->classification_;
  return ret + this->do_hash_for_method(gogo);
}

// Default implementation of do_hash_for_method.  This is appropriate
// for types with no subfields.

unsigned int
Type::do_hash_for_method(Gogo*) const
{
  return 0;
}

// Return a hash code for a string, given a starting hash.

unsigned int
Type::hash_string(const std::string& s, unsigned int h)
{
  const char* p = s.data();
  size_t len = s.length();
  for (; len > 0; --len)
    {
      h ^= *p++;
      h*= 16777619;
    }
  return h;
}

// Default check for the expression passed to make.  Any type which
// may be used with make implements its own version of this.

bool
Type::do_check_make_expression(Expression_list*, source_location)
{
  gcc_unreachable();
}

// Return whether an expression has an integer value.  Report an error
// if not.  This is used when handling calls to the predeclared make
// function.

bool
Type::check_int_value(Expression* e, const char* errmsg,
		      source_location location)
{
  if (e->type()->integer_type() != NULL)
    return true;

  // Check for a floating point constant with integer value.
  mpfr_t fval;
  mpfr_init(fval);

  Type* dummy;
  if (e->float_constant_value(fval, &dummy))
    {
      mpz_t ival;
      mpz_init(ival);

      bool ok = false;

      mpfr_clear_overflow();
      mpfr_clear_erangeflag();
      mpfr_get_z(ival, fval, GMP_RNDN);
      if (!mpfr_overflow_p()
	  && !mpfr_erangeflag_p()
	  && mpz_sgn(ival) >= 0)
	{
	  Named_type* ntype = Type::lookup_integer_type("int");
	  Integer_type* inttype = ntype->integer_type();
	  mpz_t max;
	  mpz_init_set_ui(max, 1);
	  mpz_mul_2exp(max, max, inttype->bits() - 1);
	  ok = mpz_cmp(ival, max) < 0;
	  mpz_clear(max);
	}
      mpz_clear(ival);

      if (ok)
	{
	  mpfr_clear(fval);
	  return true;
	}
    }

  mpfr_clear(fval);

  error_at(location, "%s", errmsg);
  return false;
}

// A hash table mapping unnamed types to trees.

Type::Type_trees Type::type_trees;

// Return a tree representing this type.

tree
Type::get_tree(Gogo* gogo)
{
  if (this->tree_ != NULL)
    return this->tree_;

  if (this->forward_declaration_type() != NULL
      || this->named_type() != NULL)
    return this->get_tree_without_hash(gogo);

  if (this->is_error_type())
    return error_mark_node;

  // To avoid confusing GIMPLE, we need to translate all identical Go
  // types to the same GIMPLE type.  We use a hash table to do that.
  // There is no need to use the hash table for named types, as named
  // types are only identical to themselves.

  std::pair<Type*, tree> val(this, NULL);
  std::pair<Type_trees::iterator, bool> ins =
    Type::type_trees.insert(val);
  if (!ins.second && ins.first->second != NULL_TREE)
    {
      this->tree_ = ins.first->second;
      return this->tree_;
    }

  tree t = this->get_tree_without_hash(gogo);

  if (ins.first->second == NULL_TREE)
    ins.first->second = t;
  else
    {
      // We have already created a tree for this type.  This can
      // happen when an unnamed type is defined using a named type
      // which in turns uses an identical unnamed type.  Use the tree
      // we created earlier and ignore the one we just built.
      t = ins.first->second;
      this->tree_ = t;
    }

  return t;
}

// Return a tree for a type without looking in the hash table for
// identical types.  This is used for named types, since there is no
// point to looking in the hash table for them.

tree
Type::get_tree_without_hash(Gogo* gogo)
{
  if (this->tree_ == NULL_TREE)
    {
      tree t = this->do_get_tree(gogo);

      // For a recursive function or pointer type, we will temporarily
      // return ptr_type_node during the recursion.  We don't want to
      // record that for a forwarding type, as it may confuse us
      // later.
      if (t == ptr_type_node && this->forward_declaration_type() != NULL)
	return t;

      this->tree_ = t;
      go_preserve_from_gc(t);
    }

  return this->tree_;
}

// Return a tree representing a zero initialization for this type.

tree
Type::get_init_tree(Gogo* gogo, bool is_clear)
{
  tree type_tree = this->get_tree(gogo);
  if (type_tree == error_mark_node)
    return error_mark_node;
  return this->do_get_init_tree(gogo, type_tree, is_clear);
}

// Any type which supports the builtin make function must implement
// this.

tree
Type::do_make_expression_tree(Translate_context*, Expression_list*,
			      source_location)
{
  gcc_unreachable();
}

// Return a pointer to the type descriptor for this type.

tree
Type::type_descriptor_pointer(Gogo* gogo)
{
  Type* t = this->forwarded();
  if (t->type_descriptor_decl_ == NULL_TREE)
    {
      Expression* e = t->do_type_descriptor(gogo, NULL);
      gogo->build_type_descriptor_decl(t, e, &t->type_descriptor_decl_);
      gcc_assert(t->type_descriptor_decl_ != NULL_TREE
		 && (t->type_descriptor_decl_ == error_mark_node
		     || DECL_P(t->type_descriptor_decl_)));
    }
  if (t->type_descriptor_decl_ == error_mark_node)
    return error_mark_node;
  return build_fold_addr_expr(t->type_descriptor_decl_);
}

// Return a composite literal for a type descriptor.

Expression*
Type::type_descriptor(Gogo* gogo, Type* type)
{
  return type->do_type_descriptor(gogo, NULL);
}

// Return a composite literal for a type descriptor with a name.

Expression*
Type::named_type_descriptor(Gogo* gogo, Type* type, Named_type* name)
{
  gcc_assert(name != NULL && type->named_type() != name);
  return type->do_type_descriptor(gogo, name);
}

// Make a builtin struct type from a list of fields.  The fields are
// pairs of a name and a type.

Struct_type*
Type::make_builtin_struct_type(int nfields, ...)
{
  va_list ap;
  va_start(ap, nfields);

  source_location bloc = BUILTINS_LOCATION;
  Struct_field_list* sfl = new Struct_field_list();
  for (int i = 0; i < nfields; i++)
    {
      const char* field_name = va_arg(ap, const char *);
      Type* type = va_arg(ap, Type*);
      sfl->push_back(Struct_field(Typed_identifier(field_name, type, bloc)));
    }

  va_end(ap);

  return Type::make_struct_type(sfl, bloc);
}

// Make a builtin named type.

Named_type*
Type::make_builtin_named_type(const char* name, Type* type)
{
  source_location bloc = BUILTINS_LOCATION;
  Named_object* no = Named_object::make_type(name, NULL, type, bloc);
  return no->type_value();
}

// Return the type of a type descriptor.  We should really tie this to
// runtime.Type rather than copying it.  This must match commonType in
// libgo/go/runtime/type.go.

Type*
Type::make_type_descriptor_type()
{
  static Type* ret;
  if (ret == NULL)
    {
      source_location bloc = BUILTINS_LOCATION;

      Type* uint8_type = Type::lookup_integer_type("uint8");
      Type* uint32_type = Type::lookup_integer_type("uint32");
      Type* uintptr_type = Type::lookup_integer_type("uintptr");
      Type* string_type = Type::lookup_string_type();
      Type* pointer_string_type = Type::make_pointer_type(string_type);

      // This is an unnamed version of unsafe.Pointer.  Perhaps we
      // should use the named version instead, although that would
      // require us to create the unsafe package if it has not been
      // imported.  It probably doesn't matter.
      Type* void_type = Type::make_void_type();
      Type* unsafe_pointer_type = Type::make_pointer_type(void_type);

      // Forward declaration for the type descriptor type.
      Named_object* named_type_descriptor_type =
	Named_object::make_type_declaration("commonType", NULL, bloc);
      Type* ft = Type::make_forward_declaration(named_type_descriptor_type);
      Type* pointer_type_descriptor_type = Type::make_pointer_type(ft);

      // The type of a method on a concrete type.
      Struct_type* method_type =
	Type::make_builtin_struct_type(5,
				       "name", pointer_string_type,
				       "pkgPath", pointer_string_type,
				       "mtyp", pointer_type_descriptor_type,
				       "typ", pointer_type_descriptor_type,
				       "tfn", unsafe_pointer_type);
      Named_type* named_method_type =
	Type::make_builtin_named_type("method", method_type);

      // Information for types with a name or methods.
      Type* slice_named_method_type =
	Type::make_array_type(named_method_type, NULL);
      Struct_type* uncommon_type =
	Type::make_builtin_struct_type(3,
				       "name", pointer_string_type,
				       "pkgPath", pointer_string_type,
				       "methods", slice_named_method_type);
      Named_type* named_uncommon_type =
	Type::make_builtin_named_type("uncommonType", uncommon_type);

      Type* pointer_uncommon_type =
	Type::make_pointer_type(named_uncommon_type);

      // The type descriptor type.

      Typed_identifier_list* params = new Typed_identifier_list();
      params->push_back(Typed_identifier("", unsafe_pointer_type, bloc));
      params->push_back(Typed_identifier("", uintptr_type, bloc));

      Typed_identifier_list* results = new Typed_identifier_list();
      results->push_back(Typed_identifier("", uintptr_type, bloc));

      Type* hashfn_type = Type::make_function_type(NULL, params, results, bloc);

      params = new Typed_identifier_list();
      params->push_back(Typed_identifier("", unsafe_pointer_type, bloc));
      params->push_back(Typed_identifier("", unsafe_pointer_type, bloc));
      params->push_back(Typed_identifier("", uintptr_type, bloc));

      results = new Typed_identifier_list();
      results->push_back(Typed_identifier("", Type::lookup_bool_type(), bloc));

      Type* equalfn_type = Type::make_function_type(NULL, params, results,
						    bloc);

      Struct_type* type_descriptor_type =
	Type::make_builtin_struct_type(9,
				       "Kind", uint8_type,
				       "align", uint8_type,
				       "fieldAlign", uint8_type,
				       "size", uintptr_type,
				       "hash", uint32_type,
				       "hashfn", hashfn_type,
				       "equalfn", equalfn_type,
				       "string", pointer_string_type,
				       "", pointer_uncommon_type);

      Named_type* named = Type::make_builtin_named_type("commonType",
							type_descriptor_type);

      named_type_descriptor_type->set_type_value(named);

      ret = named;
    }

  return ret;
}

// Make the type of a pointer to a type descriptor as represented in
// Go.

Type*
Type::make_type_descriptor_ptr_type()
{
  static Type* ret;
  if (ret == NULL)
    ret = Type::make_pointer_type(Type::make_type_descriptor_type());
  return ret;
}

// Return the names of runtime functions which compute a hash code for
// this type and which compare whether two values of this type are
// equal.

void
Type::type_functions(const char** hash_fn, const char** equal_fn) const
{
  switch (this->base()->classification())
    {
    case Type::TYPE_ERROR:
    case Type::TYPE_VOID:
    case Type::TYPE_NIL:
      // These types can not be hashed or compared.
      *hash_fn = "__go_type_hash_error";
      *equal_fn = "__go_type_equal_error";
      break;

    case Type::TYPE_BOOLEAN:
    case Type::TYPE_INTEGER:
    case Type::TYPE_FLOAT:
    case Type::TYPE_COMPLEX:
    case Type::TYPE_POINTER:
    case Type::TYPE_FUNCTION:
    case Type::TYPE_MAP:
    case Type::TYPE_CHANNEL:
      *hash_fn = "__go_type_hash_identity";
      *equal_fn = "__go_type_equal_identity";
      break;

    case Type::TYPE_STRING:
      *hash_fn = "__go_type_hash_string";
      *equal_fn = "__go_type_equal_string";
      break;

    case Type::TYPE_STRUCT:
    case Type::TYPE_ARRAY:
      // These types can not be hashed or compared.
      *hash_fn = "__go_type_hash_error";
      *equal_fn = "__go_type_equal_error";
      break;

    case Type::TYPE_INTERFACE:
      if (this->interface_type()->is_empty())
	{
	  *hash_fn = "__go_type_hash_empty_interface";
	  *equal_fn = "__go_type_equal_empty_interface";
	}
      else
	{
	  *hash_fn = "__go_type_hash_interface";
	  *equal_fn = "__go_type_equal_interface";
	}
      break;

    case Type::TYPE_NAMED:
    case Type::TYPE_FORWARD:
      gcc_unreachable();

    default:
      gcc_unreachable();
    }
}

// Return a composite literal for the type descriptor for a plain type
// of kind RUNTIME_TYPE_KIND named NAME.

Expression*
Type::type_descriptor_constructor(Gogo* gogo, int runtime_type_kind,
				  Named_type* name, const Methods* methods,
				  bool only_value_methods)
{
  source_location bloc = BUILTINS_LOCATION;

  Type* td_type = Type::make_type_descriptor_type();
  const Struct_field_list* fields = td_type->struct_type()->fields();

  Expression_list* vals = new Expression_list();
  vals->reserve(9);

  Struct_field_list::const_iterator p = fields->begin();
  gcc_assert(p->field_name() == "Kind");
  mpz_t iv;
  mpz_init_set_ui(iv, runtime_type_kind);
  vals->push_back(Expression::make_integer(&iv, p->type(), bloc));

  ++p;
  gcc_assert(p->field_name() == "align");
  Expression::Type_info type_info = Expression::TYPE_INFO_ALIGNMENT;
  vals->push_back(Expression::make_type_info(this, type_info));

  ++p;
  gcc_assert(p->field_name() == "fieldAlign");
  type_info = Expression::TYPE_INFO_FIELD_ALIGNMENT;
  vals->push_back(Expression::make_type_info(this, type_info));

  ++p;
  gcc_assert(p->field_name() == "size");
  type_info = Expression::TYPE_INFO_SIZE;
  vals->push_back(Expression::make_type_info(this, type_info));

  ++p;
  gcc_assert(p->field_name() == "hash");
  mpz_set_ui(iv, this->hash_for_method(gogo));
  vals->push_back(Expression::make_integer(&iv, p->type(), bloc));

  const char* hash_fn;
  const char* equal_fn;
  this->type_functions(&hash_fn, &equal_fn);

  ++p;
  gcc_assert(p->field_name() == "hashfn");
  Function_type* fntype = p->type()->function_type();
  Named_object* no = Named_object::make_function_declaration(hash_fn, NULL,
							     fntype,
							     bloc);
  no->func_declaration_value()->set_asm_name(hash_fn);
  vals->push_back(Expression::make_func_reference(no, NULL, bloc));

  ++p;
  gcc_assert(p->field_name() == "equalfn");
  fntype = p->type()->function_type();
  no = Named_object::make_function_declaration(equal_fn, NULL, fntype, bloc);
  no->func_declaration_value()->set_asm_name(equal_fn);
  vals->push_back(Expression::make_func_reference(no, NULL, bloc));

  ++p;
  gcc_assert(p->field_name() == "string");
  Expression* s = Expression::make_string((name != NULL
					   ? name->reflection(gogo)
					   : this->reflection(gogo)),
					  bloc);
  vals->push_back(Expression::make_unary(OPERATOR_AND, s, bloc));

  ++p;
  gcc_assert(p->field_name() == "uncommonType");
  if (name == NULL && methods == NULL)
    vals->push_back(Expression::make_nil(bloc));
  else
    {
      if (methods == NULL)
	methods = name->methods();
      vals->push_back(this->uncommon_type_constructor(gogo,
						      p->type()->deref(),
						      name, methods,
						      only_value_methods));
    }

  ++p;
  gcc_assert(p == fields->end());

  mpz_clear(iv);

  return Expression::make_struct_composite_literal(td_type, vals, bloc);
}

// Return a composite literal for the uncommon type information for
// this type.  UNCOMMON_STRUCT_TYPE is the type of the uncommon type
// struct.  If name is not NULL, it is the name of the type.  If
// METHODS is not NULL, it is the list of methods.  ONLY_VALUE_METHODS
// is true if only value methods should be included.  At least one of
// NAME and METHODS must not be NULL.

Expression*
Type::uncommon_type_constructor(Gogo* gogo, Type* uncommon_type,
				Named_type* name, const Methods* methods,
				bool only_value_methods) const
{
  source_location bloc = BUILTINS_LOCATION;

  const Struct_field_list* fields = uncommon_type->struct_type()->fields();

  Expression_list* vals = new Expression_list();
  vals->reserve(3);

  Struct_field_list::const_iterator p = fields->begin();
  gcc_assert(p->field_name() == "name");

  ++p;
  gcc_assert(p->field_name() == "pkgPath");

  if (name == NULL)
    {
      vals->push_back(Expression::make_nil(bloc));
      vals->push_back(Expression::make_nil(bloc));
    }
  else
    {
      Named_object* no = name->named_object();
      std::string n = Gogo::unpack_hidden_name(no->name());
      Expression* s = Expression::make_string(n, bloc);
      vals->push_back(Expression::make_unary(OPERATOR_AND, s, bloc));

      if (name->is_builtin())
	vals->push_back(Expression::make_nil(bloc));
      else
	{
	  const Package* package = no->package();
	  const std::string& unique_prefix(package == NULL
					   ? gogo->unique_prefix()
					   : package->unique_prefix());
	  const std::string& package_name(package == NULL
					  ? gogo->package_name()
					  : package->name());
	  n.assign(unique_prefix);
	  n.append(1, '.');
	  n.append(package_name);
	  if (name->in_function() != NULL)
	    {
	      n.append(1, '.');
	      n.append(Gogo::unpack_hidden_name(name->in_function()->name()));
	    }
	  s = Expression::make_string(n, bloc);
	  vals->push_back(Expression::make_unary(OPERATOR_AND, s, bloc));
	}
    }

  ++p;
  gcc_assert(p->field_name() == "methods");
  vals->push_back(this->methods_constructor(gogo, p->type(), methods,
					    only_value_methods));

  ++p;
  gcc_assert(p == fields->end());

  Expression* r = Expression::make_struct_composite_literal(uncommon_type,
							    vals, bloc);
  return Expression::make_unary(OPERATOR_AND, r, bloc);
}

// Sort methods by name.

class Sort_methods
{
 public:
  bool
  operator()(const std::pair<std::string, const Method*>& m1,
	     const std::pair<std::string, const Method*>& m2) const
  { return m1.first < m2.first; }
};

// Return a composite literal for the type method table for this type.
// METHODS_TYPE is the type of the table, and is a slice type.
// METHODS is the list of methods.  If ONLY_VALUE_METHODS is true,
// then only value methods are used.

Expression*
Type::methods_constructor(Gogo* gogo, Type* methods_type,
			  const Methods* methods,
			  bool only_value_methods) const
{
  source_location bloc = BUILTINS_LOCATION;

  std::vector<std::pair<std::string, const Method*> > smethods;
  if (methods != NULL)
    {
      smethods.reserve(methods->count());
      for (Methods::const_iterator p = methods->begin();
	   p != methods->end();
	   ++p)
	{
	  if (p->second->is_ambiguous())
	    continue;
	  if (only_value_methods && !p->second->is_value_method())
	    continue;
	  smethods.push_back(std::make_pair(p->first, p->second));
	}
    }

  if (smethods.empty())
    return Expression::make_slice_composite_literal(methods_type, NULL, bloc);

  std::sort(smethods.begin(), smethods.end(), Sort_methods());

  Type* method_type = methods_type->array_type()->element_type();

  Expression_list* vals = new Expression_list();
  vals->reserve(smethods.size());
  for (std::vector<std::pair<std::string, const Method*> >::const_iterator p
	 = smethods.begin();
       p != smethods.end();
       ++p)
    vals->push_back(this->method_constructor(gogo, method_type, p->first,
					     p->second));

  return Expression::make_slice_composite_literal(methods_type, vals, bloc);
}

// Return a composite literal for a single method.  METHOD_TYPE is the
// type of the entry.  METHOD_NAME is the name of the method and M is
// the method information.

Expression*
Type::method_constructor(Gogo*, Type* method_type,
			 const std::string& method_name,
			 const Method* m) const
{
  source_location bloc = BUILTINS_LOCATION;

  const Struct_field_list* fields = method_type->struct_type()->fields();

  Expression_list* vals = new Expression_list();
  vals->reserve(5);

  Struct_field_list::const_iterator p = fields->begin();
  gcc_assert(p->field_name() == "name");
  const std::string n = Gogo::unpack_hidden_name(method_name);
  Expression* s = Expression::make_string(n, bloc);
  vals->push_back(Expression::make_unary(OPERATOR_AND, s, bloc));

  ++p;
  gcc_assert(p->field_name() == "pkgPath");
  if (!Gogo::is_hidden_name(method_name))
    vals->push_back(Expression::make_nil(bloc));
  else
    {
      s = Expression::make_string(Gogo::hidden_name_prefix(method_name), bloc);
      vals->push_back(Expression::make_unary(OPERATOR_AND, s, bloc));
    }

  Named_object* no = (m->needs_stub_method()
		      ? m->stub_object()
		      : m->named_object());

  Function_type* mtype;
  if (no->is_function())
    mtype = no->func_value()->type();
  else
    mtype = no->func_declaration_value()->type();
  gcc_assert(mtype->is_method());
  Type* nonmethod_type = mtype->copy_without_receiver();

  ++p;
  gcc_assert(p->field_name() == "mtyp");
  vals->push_back(Expression::make_type_descriptor(nonmethod_type, bloc));

  ++p;
  gcc_assert(p->field_name() == "typ");
  vals->push_back(Expression::make_type_descriptor(mtype, bloc));

  ++p;
  gcc_assert(p->field_name() == "tfn");
  vals->push_back(Expression::make_func_reference(no, NULL, bloc));

  ++p;
  gcc_assert(p == fields->end());

  return Expression::make_struct_composite_literal(method_type, vals, bloc);
}

// Return a composite literal for the type descriptor of a plain type.
// RUNTIME_TYPE_KIND is the value of the kind field.  If NAME is not
// NULL, it is the name to use as well as the list of methods.

Expression*
Type::plain_type_descriptor(Gogo* gogo, int runtime_type_kind,
			    Named_type* name)
{
  return this->type_descriptor_constructor(gogo, runtime_type_kind,
					   name, NULL, true);
}

// Return the type reflection string for this type.

std::string
Type::reflection(Gogo* gogo) const
{
  std::string ret;

  // The do_reflection virtual function should set RET to the
  // reflection string.
  this->do_reflection(gogo, &ret);

  return ret;
}

// Return a mangled name for the type.

std::string
Type::mangled_name(Gogo* gogo) const
{
  std::string ret;

  // The do_mangled_name virtual function should set RET to the
  // mangled name.  For a composite type it should append a code for
  // the composition and then call do_mangled_name on the components.
  this->do_mangled_name(gogo, &ret);

  return ret;
}

// Default function to export a type.

void
Type::do_export(Export*) const
{
  gcc_unreachable();
}

// Import a type.

Type*
Type::import_type(Import* imp)
{
  if (imp->match_c_string("("))
    return Function_type::do_import(imp);
  else if (imp->match_c_string("*"))
    return Pointer_type::do_import(imp);
  else if (imp->match_c_string("struct "))
    return Struct_type::do_import(imp);
  else if (imp->match_c_string("["))
    return Array_type::do_import(imp);
  else if (imp->match_c_string("map "))
    return Map_type::do_import(imp);
  else if (imp->match_c_string("chan "))
    return Channel_type::do_import(imp);
  else if (imp->match_c_string("interface"))
    return Interface_type::do_import(imp);
  else
    {
      error_at(imp->location(), "import error: expected type");
      return Type::make_error_type();
    }
}

// A type used to indicate a parsing error.  This exists to simplify
// later error detection.

class Error_type : public Type
{
 public:
  Error_type()
    : Type(TYPE_ERROR)
  { }

 protected:
  tree
  do_get_tree(Gogo*)
  { return error_mark_node; }

  tree
  do_get_init_tree(Gogo*, tree, bool)
  { return error_mark_node; }

  Expression*
  do_type_descriptor(Gogo*, Named_type*)
  { return Expression::make_error(BUILTINS_LOCATION); }

  void
  do_reflection(Gogo*, std::string*) const
  { gcc_assert(saw_errors()); }

  void
  do_mangled_name(Gogo*, std::string* ret) const
  { ret->push_back('E'); }
};

Type*
Type::make_error_type()
{
  static Error_type singleton_error_type;
  return &singleton_error_type;
}

// The void type.

class Void_type : public Type
{
 public:
  Void_type()
    : Type(TYPE_VOID)
  { }

 protected:
  tree
  do_get_tree(Gogo*)
  { return void_type_node; }

  tree
  do_get_init_tree(Gogo*, tree, bool)
  { gcc_unreachable(); }

  Expression*
  do_type_descriptor(Gogo*, Named_type*)
  { gcc_unreachable(); }

  void
  do_reflection(Gogo*, std::string*) const
  { }

  void
  do_mangled_name(Gogo*, std::string* ret) const
  { ret->push_back('v'); }
};

Type*
Type::make_void_type()
{
  static Void_type singleton_void_type;
  return &singleton_void_type;
}

// The boolean type.

class Boolean_type : public Type
{
 public:
  Boolean_type()
    : Type(TYPE_BOOLEAN)
  { }

 protected:
  tree
  do_get_tree(Gogo*)
  { return boolean_type_node; }

  tree
  do_get_init_tree(Gogo*, tree type_tree, bool is_clear)
  { return is_clear ? NULL : fold_convert(type_tree, boolean_false_node); }

  Expression*
  do_type_descriptor(Gogo*, Named_type* name);

  // We should not be asked for the reflection string of a basic type.
  void
  do_reflection(Gogo*, std::string* ret) const
  { ret->append("bool"); }

  void
  do_mangled_name(Gogo*, std::string* ret) const
  { ret->push_back('b'); }
};

// Make the type descriptor.

Expression*
Boolean_type::do_type_descriptor(Gogo* gogo, Named_type* name)
{
  if (name != NULL)
    return this->plain_type_descriptor(gogo, RUNTIME_TYPE_KIND_BOOL, name);
  else
    {
      Named_object* no = gogo->lookup_global("bool");
      gcc_assert(no != NULL);
      return Type::type_descriptor(gogo, no->type_value());
    }
}

Type*
Type::make_boolean_type()
{
  static Boolean_type boolean_type;
  return &boolean_type;
}

// The named type "bool".

static Named_type* named_bool_type;

// Get the named type "bool".

Named_type*
Type::lookup_bool_type()
{
  return named_bool_type;
}

// Make the named type "bool".

Named_type*
Type::make_named_bool_type()
{
  Type* bool_type = Type::make_boolean_type();
  Named_object* named_object = Named_object::make_type("bool", NULL,
						       bool_type,
						       BUILTINS_LOCATION);
  Named_type* named_type = named_object->type_value();
  named_bool_type = named_type;
  return named_type;
}

// Class Integer_type.

Integer_type::Named_integer_types Integer_type::named_integer_types;

// Create a new integer type.  Non-abstract integer types always have
// names.

Named_type*
Integer_type::create_integer_type(const char* name, bool is_unsigned,
				  int bits, int runtime_type_kind)
{
  Integer_type* integer_type = new Integer_type(false, is_unsigned, bits,
						runtime_type_kind);
  std::string sname(name);
  Named_object* named_object = Named_object::make_type(sname, NULL,
						       integer_type,
						       BUILTINS_LOCATION);
  Named_type* named_type = named_object->type_value();
  std::pair<Named_integer_types::iterator, bool> ins =
    Integer_type::named_integer_types.insert(std::make_pair(sname, named_type));
  gcc_assert(ins.second);
  return named_type;
}

// Look up an existing integer type.

Named_type*
Integer_type::lookup_integer_type(const char* name)
{
  Named_integer_types::const_iterator p =
    Integer_type::named_integer_types.find(name);
  gcc_assert(p != Integer_type::named_integer_types.end());
  return p->second;
}

// Create a new abstract integer type.

Integer_type*
Integer_type::create_abstract_integer_type()
{
  static Integer_type* abstract_type;
  if (abstract_type == NULL)
    abstract_type = new Integer_type(true, false, INT_TYPE_SIZE,
				     RUNTIME_TYPE_KIND_INT);
  return abstract_type;
}

// Integer type compatibility.

bool
Integer_type::is_identical(const Integer_type* t) const
{
  if (this->is_unsigned_ != t->is_unsigned_ || this->bits_ != t->bits_)
    return false;
  return this->is_abstract_ == t->is_abstract_;
}

// Hash code.

unsigned int
Integer_type::do_hash_for_method(Gogo*) const
{
  return ((this->bits_ << 4)
	  + ((this->is_unsigned_ ? 1 : 0) << 8)
	  + ((this->is_abstract_ ? 1 : 0) << 9));
}

// Get the tree for an Integer_type.

tree
Integer_type::do_get_tree(Gogo*)
{
  gcc_assert(!this->is_abstract_);
  if (this->is_unsigned_)
    {
      if (this->bits_ == INT_TYPE_SIZE)
	return unsigned_type_node;
      else if (this->bits_ == CHAR_TYPE_SIZE)
	return unsigned_char_type_node;
      else if (this->bits_ == SHORT_TYPE_SIZE)
	return short_unsigned_type_node;
      else if (this->bits_ == LONG_TYPE_SIZE)
	return long_unsigned_type_node;
      else if (this->bits_ == LONG_LONG_TYPE_SIZE)
	return long_long_unsigned_type_node;
      else
	return make_unsigned_type(this->bits_);
    }
  else
    {
      if (this->bits_ == INT_TYPE_SIZE)
	return integer_type_node;
      else if (this->bits_ == CHAR_TYPE_SIZE)
	return signed_char_type_node;
      else if (this->bits_ == SHORT_TYPE_SIZE)
	return short_integer_type_node;
      else if (this->bits_ == LONG_TYPE_SIZE)
	return long_integer_type_node;
      else if (this->bits_ == LONG_LONG_TYPE_SIZE)
	return long_long_integer_type_node;
      else
	return make_signed_type(this->bits_);
    }
}

tree
Integer_type::do_get_init_tree(Gogo*, tree type_tree, bool is_clear)
{
  return is_clear ? NULL : build_int_cst(type_tree, 0);
}

// The type descriptor for an integer type.  Integer types are always
// named.

Expression*
Integer_type::do_type_descriptor(Gogo* gogo, Named_type* name)
{
  gcc_assert(name != NULL);
  return this->plain_type_descriptor(gogo, this->runtime_type_kind_, name);
}

// We should not be asked for the reflection string of a basic type.

void
Integer_type::do_reflection(Gogo*, std::string*) const
{
  gcc_unreachable();
}

// Mangled name.

void
Integer_type::do_mangled_name(Gogo*, std::string* ret) const
{
  char buf[100];
  snprintf(buf, sizeof buf, "i%s%s%de",
	   this->is_abstract_ ? "a" : "",
	   this->is_unsigned_ ? "u" : "",
	   this->bits_);
  ret->append(buf);
}

// Make an integer type.

Named_type*
Type::make_integer_type(const char* name, bool is_unsigned, int bits,
			int runtime_type_kind)
{
  return Integer_type::create_integer_type(name, is_unsigned, bits,
					   runtime_type_kind);
}

// Make an abstract integer type.

Integer_type*
Type::make_abstract_integer_type()
{
  return Integer_type::create_abstract_integer_type();
}

// Look up an integer type.

Named_type*
Type::lookup_integer_type(const char* name)
{
  return Integer_type::lookup_integer_type(name);
}

// Class Float_type.

Float_type::Named_float_types Float_type::named_float_types;

// Create a new float type.  Non-abstract float types always have
// names.

Named_type*
Float_type::create_float_type(const char* name, int bits,
			      int runtime_type_kind)
{
  Float_type* float_type = new Float_type(false, bits, runtime_type_kind);
  std::string sname(name);
  Named_object* named_object = Named_object::make_type(sname, NULL, float_type,
						       BUILTINS_LOCATION);
  Named_type* named_type = named_object->type_value();
  std::pair<Named_float_types::iterator, bool> ins =
    Float_type::named_float_types.insert(std::make_pair(sname, named_type));
  gcc_assert(ins.second);
  return named_type;
}

// Look up an existing float type.

Named_type*
Float_type::lookup_float_type(const char* name)
{
  Named_float_types::const_iterator p =
    Float_type::named_float_types.find(name);
  gcc_assert(p != Float_type::named_float_types.end());
  return p->second;
}

// Create a new abstract float type.

Float_type*
Float_type::create_abstract_float_type()
{
  static Float_type* abstract_type;
  if (abstract_type == NULL)
    abstract_type = new Float_type(true, 64, RUNTIME_TYPE_KIND_FLOAT64);
  return abstract_type;
}

// Whether this type is identical with T.

bool
Float_type::is_identical(const Float_type* t) const
{
  if (this->bits_ != t->bits_)
    return false;
  return this->is_abstract_ == t->is_abstract_;
}

// Hash code.

unsigned int
Float_type::do_hash_for_method(Gogo*) const
{
  return (this->bits_ << 4) + ((this->is_abstract_ ? 1 : 0) << 8);
}

// Get a tree without using a Gogo*.

tree
Float_type::type_tree() const
{
  if (this->bits_ == FLOAT_TYPE_SIZE)
    return float_type_node;
  else if (this->bits_ == DOUBLE_TYPE_SIZE)
    return double_type_node;
  else if (this->bits_ == LONG_DOUBLE_TYPE_SIZE)
    return long_double_type_node;
  else
    {
      tree ret = make_node(REAL_TYPE);
      TYPE_PRECISION(ret) = this->bits_;
      layout_type(ret);
      return ret;
    }
}

// Get a tree.

tree
Float_type::do_get_tree(Gogo*)
{
  return this->type_tree();
}

tree
Float_type::do_get_init_tree(Gogo*, tree type_tree, bool is_clear)
{
  if (is_clear)
    return NULL;
  REAL_VALUE_TYPE r;
  real_from_integer(&r, TYPE_MODE(type_tree), 0, 0, 0);
  return build_real(type_tree, r);
}

// The type descriptor for a float type.  Float types are always named.

Expression*
Float_type::do_type_descriptor(Gogo* gogo, Named_type* name)
{
  gcc_assert(name != NULL);
  return this->plain_type_descriptor(gogo, this->runtime_type_kind_, name);
}

// We should not be asked for the reflection string of a basic type.

void
Float_type::do_reflection(Gogo*, std::string*) const
{
  gcc_unreachable();
}

// Mangled name.

void
Float_type::do_mangled_name(Gogo*, std::string* ret) const
{
  char buf[100];
  snprintf(buf, sizeof buf, "f%s%de",
	   this->is_abstract_ ? "a" : "",
	   this->bits_);
  ret->append(buf);
}

// Make a floating point type.

Named_type*
Type::make_float_type(const char* name, int bits, int runtime_type_kind)
{
  return Float_type::create_float_type(name, bits, runtime_type_kind);
}

// Make an abstract float type.

Float_type*
Type::make_abstract_float_type()
{
  return Float_type::create_abstract_float_type();
}

// Look up a float type.

Named_type*
Type::lookup_float_type(const char* name)
{
  return Float_type::lookup_float_type(name);
}

// Class Complex_type.

Complex_type::Named_complex_types Complex_type::named_complex_types;

// Create a new complex type.  Non-abstract complex types always have
// names.

Named_type*
Complex_type::create_complex_type(const char* name, int bits,
				  int runtime_type_kind)
{
  Complex_type* complex_type = new Complex_type(false, bits,
						runtime_type_kind);
  std::string sname(name);
  Named_object* named_object = Named_object::make_type(sname, NULL,
						       complex_type,
						       BUILTINS_LOCATION);
  Named_type* named_type = named_object->type_value();
  std::pair<Named_complex_types::iterator, bool> ins =
    Complex_type::named_complex_types.insert(std::make_pair(sname,
							    named_type));
  gcc_assert(ins.second);
  return named_type;
}

// Look up an existing complex type.

Named_type*
Complex_type::lookup_complex_type(const char* name)
{
  Named_complex_types::const_iterator p =
    Complex_type::named_complex_types.find(name);
  gcc_assert(p != Complex_type::named_complex_types.end());
  return p->second;
}

// Create a new abstract complex type.

Complex_type*
Complex_type::create_abstract_complex_type()
{
  static Complex_type* abstract_type;
  if (abstract_type == NULL)
    abstract_type = new Complex_type(true, 128, RUNTIME_TYPE_KIND_COMPLEX128);
  return abstract_type;
}

// Whether this type is identical with T.

bool
Complex_type::is_identical(const Complex_type *t) const
{
  if (this->bits_ != t->bits_)
    return false;
  return this->is_abstract_ == t->is_abstract_;
}

// Hash code.

unsigned int
Complex_type::do_hash_for_method(Gogo*) const
{
  return (this->bits_ << 4) + ((this->is_abstract_ ? 1 : 0) << 8);
}

// Get a tree without using a Gogo*.

tree
Complex_type::type_tree() const
{
  if (this->bits_ == FLOAT_TYPE_SIZE * 2)
    return complex_float_type_node;
  else if (this->bits_ == DOUBLE_TYPE_SIZE * 2)
    return complex_double_type_node;
  else if (this->bits_ == LONG_DOUBLE_TYPE_SIZE * 2)
    return complex_long_double_type_node;
  else
    {
      tree ret = make_node(REAL_TYPE);
      TYPE_PRECISION(ret) = this->bits_ / 2;
      layout_type(ret);
      return build_complex_type(ret);
    }
}

// Get a tree.

tree
Complex_type::do_get_tree(Gogo*)
{
  return this->type_tree();
}

// Zero initializer.

tree
Complex_type::do_get_init_tree(Gogo*, tree type_tree, bool is_clear)
{
  if (is_clear)
    return NULL;
  REAL_VALUE_TYPE r;
  real_from_integer(&r, TYPE_MODE(TREE_TYPE(type_tree)), 0, 0, 0);
  return build_complex(type_tree, build_real(TREE_TYPE(type_tree), r),
		       build_real(TREE_TYPE(type_tree), r));
}

// The type descriptor for a complex type.  Complex types are always
// named.

Expression*
Complex_type::do_type_descriptor(Gogo* gogo, Named_type* name)
{
  gcc_assert(name != NULL);
  return this->plain_type_descriptor(gogo, this->runtime_type_kind_, name);
}

// We should not be asked for the reflection string of a basic type.

void
Complex_type::do_reflection(Gogo*, std::string*) const
{
  gcc_unreachable();
}

// Mangled name.

void
Complex_type::do_mangled_name(Gogo*, std::string* ret) const
{
  char buf[100];
  snprintf(buf, sizeof buf, "c%s%de",
	   this->is_abstract_ ? "a" : "",
	   this->bits_);
  ret->append(buf);
}

// Make a complex type.

Named_type*
Type::make_complex_type(const char* name, int bits, int runtime_type_kind)
{
  return Complex_type::create_complex_type(name, bits, runtime_type_kind);
}

// Make an abstract complex type.

Complex_type*
Type::make_abstract_complex_type()
{
  return Complex_type::create_abstract_complex_type();
}

// Look up a complex type.

Named_type*
Type::lookup_complex_type(const char* name)
{
  return Complex_type::lookup_complex_type(name);
}

// Class String_type.

// Return the tree for String_type.  A string is a struct with two
// fields: a pointer to the characters and a length.

tree
String_type::do_get_tree(Gogo*)
{
  static tree struct_type;
  return Gogo::builtin_struct(&struct_type, "__go_string", NULL_TREE, 2,
			      "__data",
			      build_pointer_type(unsigned_char_type_node),
			      "__length",
			      integer_type_node);
}

// Return a tree for the length of STRING.

tree
String_type::length_tree(Gogo*, tree string)
{
  tree string_type = TREE_TYPE(string);
  gcc_assert(TREE_CODE(string_type) == RECORD_TYPE);
  tree length_field = DECL_CHAIN(TYPE_FIELDS(string_type));
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(length_field)),
		    "__length") == 0);
  return fold_build3(COMPONENT_REF, integer_type_node, string,
		     length_field, NULL_TREE);
}

// Return a tree for a pointer to the bytes of STRING.

tree
String_type::bytes_tree(Gogo*, tree string)
{
  tree string_type = TREE_TYPE(string);
  gcc_assert(TREE_CODE(string_type) == RECORD_TYPE);
  tree bytes_field = TYPE_FIELDS(string_type);
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(bytes_field)),
		    "__data") == 0);
  return fold_build3(COMPONENT_REF, TREE_TYPE(bytes_field), string,
		     bytes_field, NULL_TREE);
}

// We initialize a string to { NULL, 0 }.

tree
String_type::do_get_init_tree(Gogo*, tree type_tree, bool is_clear)
{
  if (is_clear)
    return NULL_TREE;

  gcc_assert(TREE_CODE(type_tree) == RECORD_TYPE);

  VEC(constructor_elt, gc)* init = VEC_alloc(constructor_elt, gc, 2);

  for (tree field = TYPE_FIELDS(type_tree);
       field != NULL_TREE;
       field = DECL_CHAIN(field))
    {
      constructor_elt* elt = VEC_quick_push(constructor_elt, init, NULL);
      elt->index = field;
      elt->value = fold_convert(TREE_TYPE(field), size_zero_node);
    }

  tree ret = build_constructor(type_tree, init);
  TREE_CONSTANT(ret) = 1;
  return ret;
}

// The type descriptor for the string type.

Expression*
String_type::do_type_descriptor(Gogo* gogo, Named_type* name)
{
  if (name != NULL)
    return this->plain_type_descriptor(gogo, RUNTIME_TYPE_KIND_STRING, name);
  else
    {
      Named_object* no = gogo->lookup_global("string");
      gcc_assert(no != NULL);
      return Type::type_descriptor(gogo, no->type_value());
    }
}

// We should not be asked for the reflection string of a basic type.

void
String_type::do_reflection(Gogo*, std::string* ret) const
{
  ret->append("string");
}

// Mangled name of a string type.

void
String_type::do_mangled_name(Gogo*, std::string* ret) const
{
  ret->push_back('z');
}

// Make a string type.

Type*
Type::make_string_type()
{
  static String_type string_type;
  return &string_type;
}

// The named type "string".

static Named_type* named_string_type;

// Get the named type "string".

Named_type*
Type::lookup_string_type()
{
  return named_string_type;
}

// Make the named type string.

Named_type*
Type::make_named_string_type()
{
  Type* string_type = Type::make_string_type();
  Named_object* named_object = Named_object::make_type("string", NULL,
						       string_type,
						       BUILTINS_LOCATION);
  Named_type* named_type = named_object->type_value();
  named_string_type = named_type;
  return named_type;
}

// The sink type.  This is the type of the blank identifier _.  Any
// type may be assigned to it.

class Sink_type : public Type
{
 public:
  Sink_type()
    : Type(TYPE_SINK)
  { }

 protected:
  tree
  do_get_tree(Gogo*)
  { gcc_unreachable(); }

  tree
  do_get_init_tree(Gogo*, tree, bool)
  { gcc_unreachable(); }

  Expression*
  do_type_descriptor(Gogo*, Named_type*)
  { gcc_unreachable(); }

  void
  do_reflection(Gogo*, std::string*) const
  { gcc_unreachable(); }

  void
  do_mangled_name(Gogo*, std::string*) const
  { gcc_unreachable(); }
};

// Make the sink type.

Type*
Type::make_sink_type()
{
  static Sink_type sink_type;
  return &sink_type;
}

// Class Function_type.

// Traversal.

int
Function_type::do_traverse(Traverse* traverse)
{
  if (this->receiver_ != NULL
      && Type::traverse(this->receiver_->type(), traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (this->parameters_ != NULL
      && this->parameters_->traverse(traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (this->results_ != NULL
      && this->results_->traverse(traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Returns whether T is a valid redeclaration of this type.  If this
// returns false, and REASON is not NULL, *REASON may be set to a
// brief explanation of why it returned false.

bool
Function_type::is_valid_redeclaration(const Function_type* t,
				      std::string* reason) const
{
  if (!this->is_identical(t, false, true, reason))
    return false;

  // A redeclaration of a function is required to use the same names
  // for the receiver and parameters.
  if (this->receiver() != NULL
      && this->receiver()->name() != t->receiver()->name()
      && this->receiver()->name() != Import::import_marker
      && t->receiver()->name() != Import::import_marker)
    {
      if (reason != NULL)
	*reason = "receiver name changed";
      return false;
    }

  const Typed_identifier_list* parms1 = this->parameters();
  const Typed_identifier_list* parms2 = t->parameters();
  if (parms1 != NULL)
    {
      Typed_identifier_list::const_iterator p1 = parms1->begin();
      for (Typed_identifier_list::const_iterator p2 = parms2->begin();
	   p2 != parms2->end();
	   ++p2, ++p1)
	{
	  if (p1->name() != p2->name()
	      && p1->name() != Import::import_marker
	      && p2->name() != Import::import_marker)
	    {
	      if (reason != NULL)
		*reason = "parameter name changed";
	      return false;
	    }

	  // This is called at parse time, so we may have unknown
	  // types.
	  Type* t1 = p1->type()->forwarded();
	  Type* t2 = p2->type()->forwarded();
	  if (t1 != t2
	      && t1->forward_declaration_type() != NULL
	      && (t2->forward_declaration_type() == NULL
		  || (t1->forward_declaration_type()->named_object()
		      != t2->forward_declaration_type()->named_object())))
	    return false;
	}
    }

  const Typed_identifier_list* results1 = this->results();
  const Typed_identifier_list* results2 = t->results();
  if (results1 != NULL)
    {
      Typed_identifier_list::const_iterator res1 = results1->begin();
      for (Typed_identifier_list::const_iterator res2 = results2->begin();
	   res2 != results2->end();
	   ++res2, ++res1)
	{
	  if (res1->name() != res2->name()
	      && res1->name() != Import::import_marker
	      && res2->name() != Import::import_marker)
	    {
	      if (reason != NULL)
		*reason = "result name changed";
	      return false;
	    }

	  // This is called at parse time, so we may have unknown
	  // types.
	  Type* t1 = res1->type()->forwarded();
	  Type* t2 = res2->type()->forwarded();
	  if (t1 != t2
	      && t1->forward_declaration_type() != NULL
	      && (t2->forward_declaration_type() == NULL
		  || (t1->forward_declaration_type()->named_object()
		      != t2->forward_declaration_type()->named_object())))
	    return false;
	}
    }

  return true;
}

// Check whether T is the same as this type.

bool
Function_type::is_identical(const Function_type* t, bool ignore_receiver,
			    bool errors_are_identical,
			    std::string* reason) const
{
  if (!ignore_receiver)
    {
      const Typed_identifier* r1 = this->receiver();
      const Typed_identifier* r2 = t->receiver();
      if ((r1 != NULL) != (r2 != NULL))
	{
	  if (reason != NULL)
	    *reason = _("different receiver types");
	  return false;
	}
      if (r1 != NULL)
	{
	  if (!Type::are_identical(r1->type(), r2->type(), errors_are_identical,
				   reason))
	    {
	      if (reason != NULL && !reason->empty())
		*reason = "receiver: " + *reason;
	      return false;
	    }
	}
    }

  const Typed_identifier_list* parms1 = this->parameters();
  const Typed_identifier_list* parms2 = t->parameters();
  if ((parms1 != NULL) != (parms2 != NULL))
    {
      if (reason != NULL)
	*reason = _("different number of parameters");
      return false;
    }
  if (parms1 != NULL)
    {
      Typed_identifier_list::const_iterator p1 = parms1->begin();
      for (Typed_identifier_list::const_iterator p2 = parms2->begin();
	   p2 != parms2->end();
	   ++p2, ++p1)
	{
	  if (p1 == parms1->end())
	    {
	      if (reason != NULL)
		*reason = _("different number of parameters");
	      return false;
	    }

	  if (!Type::are_identical(p1->type(), p2->type(),
				   errors_are_identical, NULL))
	    {
	      if (reason != NULL)
		*reason = _("different parameter types");
	      return false;
	    }
	}
      if (p1 != parms1->end())
	{
	  if (reason != NULL)
	    *reason = _("different number of parameters");
	return false;
	}
    }

  if (this->is_varargs() != t->is_varargs())
    {
      if (reason != NULL)
	*reason = _("different varargs");
      return false;
    }

  const Typed_identifier_list* results1 = this->results();
  const Typed_identifier_list* results2 = t->results();
  if ((results1 != NULL) != (results2 != NULL))
    {
      if (reason != NULL)
	*reason = _("different number of results");
      return false;
    }
  if (results1 != NULL)
    {
      Typed_identifier_list::const_iterator res1 = results1->begin();
      for (Typed_identifier_list::const_iterator res2 = results2->begin();
	   res2 != results2->end();
	   ++res2, ++res1)
	{
	  if (res1 == results1->end())
	    {
	      if (reason != NULL)
		*reason = _("different number of results");
	      return false;
	    }

	  if (!Type::are_identical(res1->type(), res2->type(),
				   errors_are_identical, NULL))
	    {
	      if (reason != NULL)
		*reason = _("different result types");
	      return false;
	    }
	}
      if (res1 != results1->end())
	{
	  if (reason != NULL)
	    *reason = _("different number of results");
	  return false;
	}
    }

  return true;
}

// Hash code.

unsigned int
Function_type::do_hash_for_method(Gogo* gogo) const
{
  unsigned int ret = 0;
  // We ignore the receiver type for hash codes, because we need to
  // get the same hash code for a method in an interface and a method
  // declared for a type.  The former will not have a receiver.
  if (this->parameters_ != NULL)
    {
      int shift = 1;
      for (Typed_identifier_list::const_iterator p = this->parameters_->begin();
	   p != this->parameters_->end();
	   ++p, ++shift)
	ret += p->type()->hash_for_method(gogo) << shift;
    }
  if (this->results_ != NULL)
    {
      int shift = 2;
      for (Typed_identifier_list::const_iterator p = this->results_->begin();
	   p != this->results_->end();
	   ++p, ++shift)
	ret += p->type()->hash_for_method(gogo) << shift;
    }
  if (this->is_varargs_)
    ret += 1;
  ret <<= 4;
  return ret;
}

// Get the tree for a function type.

tree
Function_type::do_get_tree(Gogo* gogo)
{
  tree args = NULL_TREE;
  tree* pp = &args;

  if (this->receiver_ != NULL)
    {
      Type* rtype = this->receiver_->type();
      tree ptype = rtype->get_tree(gogo);
      if (ptype == error_mark_node)
	return error_mark_node;

      // We always pass the address of the receiver parameter, in
      // order to make interface calls work with unknown types.
      if (rtype->points_to() == NULL)
	ptype = build_pointer_type(ptype);

      *pp = tree_cons (NULL_TREE, ptype, NULL_TREE);
      pp = &TREE_CHAIN (*pp);
    }

  if (this->parameters_ != NULL)
    {
      for (Typed_identifier_list::const_iterator p = this->parameters_->begin();
	   p != this->parameters_->end();
	   ++p)
	{
	  tree ptype = p->type()->get_tree(gogo);
	  if (ptype == error_mark_node)
	    return error_mark_node;
	  *pp = tree_cons (NULL_TREE, ptype, NULL_TREE);
	  pp = &TREE_CHAIN (*pp);
	}
    }

  // Varargs is handled entirely at the Go level.  At the tree level,
  // functions are not varargs.
  *pp = void_list_node;

  tree result;
  if (this->results_ == NULL)
    result = void_type_node;
  else if (this->results_->size() == 1)
    result = this->results_->begin()->type()->get_tree(gogo);
  else
    {
      result = make_node(RECORD_TYPE);
      tree field_trees = NULL_TREE;
      tree* pp = &field_trees;
      for (Typed_identifier_list::const_iterator p = this->results_->begin();
	   p != this->results_->end();
	   ++p)
	{
	  const std::string name = (p->name().empty()
				    ? "UNNAMED"
				    : Gogo::unpack_hidden_name(p->name()));
	  tree name_tree = get_identifier_with_length(name.data(),
						      name.length());
	  tree field_type_tree = p->type()->get_tree(gogo);
	  if (field_type_tree == error_mark_node)
	    return error_mark_node;
	  tree field = build_decl(this->location_, FIELD_DECL, name_tree,
				  field_type_tree);
	  DECL_CONTEXT(field) = result;
	  *pp = field;
	  pp = &DECL_CHAIN(field);
	}
      TYPE_FIELDS(result) = field_trees;
      layout_type(result);
    }

  if (result == error_mark_node)
    return error_mark_node;

  tree fntype = build_function_type(result, args);
  if (fntype == error_mark_node)
    return fntype;

  return build_pointer_type(fntype);
}

// Functions are initialized to NULL.

tree
Function_type::do_get_init_tree(Gogo*, tree type_tree, bool is_clear)
{
  if (is_clear)
    return NULL;
  return fold_convert(type_tree, null_pointer_node);
}

// The type of a function type descriptor.

Type*
Function_type::make_function_type_descriptor_type()
{
  static Type* ret;
  if (ret == NULL)
    {
      Type* tdt = Type::make_type_descriptor_type();
      Type* ptdt = Type::make_type_descriptor_ptr_type();

      Type* bool_type = Type::lookup_bool_type();

      Type* slice_type = Type::make_array_type(ptdt, NULL);

      Struct_type* s = Type::make_builtin_struct_type(4,
						      "", tdt,
						      "dotdotdot", bool_type,
						      "in", slice_type,
						      "out", slice_type);

      ret = Type::make_builtin_named_type("FuncType", s);
    }

  return ret;
}

// The type descriptor for a function type.

Expression*
Function_type::do_type_descriptor(Gogo* gogo, Named_type* name)
{
  source_location bloc = BUILTINS_LOCATION;

  Type* ftdt = Function_type::make_function_type_descriptor_type();

  const Struct_field_list* fields = ftdt->struct_type()->fields();

  Expression_list* vals = new Expression_list();
  vals->reserve(4);

  Struct_field_list::const_iterator p = fields->begin();
  gcc_assert(p->field_name() == "commonType");
  vals->push_back(this->type_descriptor_constructor(gogo,
						    RUNTIME_TYPE_KIND_FUNC,
						    name, NULL, true));

  ++p;
  gcc_assert(p->field_name() == "dotdotdot");
  vals->push_back(Expression::make_boolean(this->is_varargs(), bloc));

  ++p;
  gcc_assert(p->field_name() == "in");
  vals->push_back(this->type_descriptor_params(p->type(), this->receiver(),
					       this->parameters()));

  ++p;
  gcc_assert(p->field_name() == "out");
  vals->push_back(this->type_descriptor_params(p->type(), NULL,
					       this->results()));

  ++p;
  gcc_assert(p == fields->end());

  return Expression::make_struct_composite_literal(ftdt, vals, bloc);
}

// Return a composite literal for the parameters or results of a type
// descriptor.

Expression*
Function_type::type_descriptor_params(Type* params_type,
				      const Typed_identifier* receiver,
				      const Typed_identifier_list* params)
{
  source_location bloc = BUILTINS_LOCATION;

  if (receiver == NULL && params == NULL)
    return Expression::make_slice_composite_literal(params_type, NULL, bloc);

  Expression_list* vals = new Expression_list();
  vals->reserve((params == NULL ? 0 : params->size())
		+ (receiver != NULL ? 1 : 0));

  if (receiver != NULL)
    {
      Type* rtype = receiver->type();
      // The receiver is always passed as a pointer.  FIXME: Is this
      // right?  Should that fact affect the type descriptor?
      if (rtype->points_to() == NULL)
	rtype = Type::make_pointer_type(rtype);
      vals->push_back(Expression::make_type_descriptor(rtype, bloc));
    }

  if (params != NULL)
    {
      for (Typed_identifier_list::const_iterator p = params->begin();
	   p != params->end();
	   ++p)
	vals->push_back(Expression::make_type_descriptor(p->type(), bloc));
    }

  return Expression::make_slice_composite_literal(params_type, vals, bloc);
}

// The reflection string.

void
Function_type::do_reflection(Gogo* gogo, std::string* ret) const
{
  // FIXME: Turn this off until we straighten out the type of the
  // struct field used in a go statement which calls a method.
  // gcc_assert(this->receiver_ == NULL);

  ret->append("func");

  if (this->receiver_ != NULL)
    {
      ret->push_back('(');
      this->append_reflection(this->receiver_->type(), gogo, ret);
      ret->push_back(')');
    }

  ret->push_back('(');
  const Typed_identifier_list* params = this->parameters();
  if (params != NULL)
    {
      bool is_varargs = this->is_varargs_;
      for (Typed_identifier_list::const_iterator p = params->begin();
	   p != params->end();
	   ++p)
	{
	  if (p != params->begin())
	    ret->append(", ");
	  if (!is_varargs || p + 1 != params->end())
	    this->append_reflection(p->type(), gogo, ret);
	  else
	    {
	      ret->append("...");
	      this->append_reflection(p->type()->array_type()->element_type(),
				      gogo, ret);
	    }
	}
    }
  ret->push_back(')');

  const Typed_identifier_list* results = this->results();
  if (results != NULL && !results->empty())
    {
      if (results->size() == 1)
	ret->push_back(' ');
      else
	ret->append(" (");
      for (Typed_identifier_list::const_iterator p = results->begin();
	   p != results->end();
	   ++p)
	{
	  if (p != results->begin())
	    ret->append(", ");
	  this->append_reflection(p->type(), gogo, ret);
	}
      if (results->size() > 1)
	ret->push_back(')');
    }
}

// Mangled name.

void
Function_type::do_mangled_name(Gogo* gogo, std::string* ret) const
{
  ret->push_back('F');

  if (this->receiver_ != NULL)
    {
      ret->push_back('m');
      this->append_mangled_name(this->receiver_->type(), gogo, ret);
    }

  const Typed_identifier_list* params = this->parameters();
  if (params != NULL)
    {
      ret->push_back('p');
      for (Typed_identifier_list::const_iterator p = params->begin();
	   p != params->end();
	   ++p)
	this->append_mangled_name(p->type(), gogo, ret);
      if (this->is_varargs_)
	ret->push_back('V');
      ret->push_back('e');
    }

  const Typed_identifier_list* results = this->results();
  if (results != NULL)
    {
      ret->push_back('r');
      for (Typed_identifier_list::const_iterator p = results->begin();
	   p != results->end();
	   ++p)
	this->append_mangled_name(p->type(), gogo, ret);
      ret->push_back('e');
    }

  ret->push_back('e');
}

// Export a function type.

void
Function_type::do_export(Export* exp) const
{
  // We don't write out the receiver.  The only function types which
  // should have a receiver are the ones associated with explicitly
  // defined methods.  For those the receiver type is written out by
  // Function::export_func.

  exp->write_c_string("(");
  bool first = true;
  if (this->parameters_ != NULL)
    {
      bool is_varargs = this->is_varargs_;
      for (Typed_identifier_list::const_iterator p =
	     this->parameters_->begin();
	   p != this->parameters_->end();
	   ++p)
	{
	  if (first)
	    first = false;
	  else
	    exp->write_c_string(", ");
	  if (!is_varargs || p + 1 != this->parameters_->end())
	    exp->write_type(p->type());
	  else
	    {
	      exp->write_c_string("...");
	      exp->write_type(p->type()->array_type()->element_type());
	    }
	}
    }
  exp->write_c_string(")");

  const Typed_identifier_list* results = this->results_;
  if (results != NULL)
    {
      exp->write_c_string(" ");
      if (results->size() == 1)
	exp->write_type(results->begin()->type());
      else
	{
	  first = true;
	  exp->write_c_string("(");
	  for (Typed_identifier_list::const_iterator p = results->begin();
	       p != results->end();
	       ++p)
	    {
	      if (first)
		first = false;
	      else
		exp->write_c_string(", ");
	      exp->write_type(p->type());
	    }
	  exp->write_c_string(")");
	}
    }
}

// Import a function type.

Function_type*
Function_type::do_import(Import* imp)
{
  imp->require_c_string("(");
  Typed_identifier_list* parameters;
  bool is_varargs = false;
  if (imp->peek_char() == ')')
    parameters = NULL;
  else
    {
      parameters = new Typed_identifier_list();
      while (true)
	{
	  if (imp->match_c_string("..."))
	    {
	      imp->advance(3);
	      is_varargs = true;
	    }

	  Type* ptype = imp->read_type();
	  if (is_varargs)
	    ptype = Type::make_array_type(ptype, NULL);
	  parameters->push_back(Typed_identifier(Import::import_marker,
						 ptype, imp->location()));
	  if (imp->peek_char() != ',')
	    break;
	  gcc_assert(!is_varargs);
	  imp->require_c_string(", ");
	}
    }
  imp->require_c_string(")");

  Typed_identifier_list* results;
  if (imp->peek_char() != ' ')
    results = NULL;
  else
    {
      imp->advance(1);
      results = new Typed_identifier_list;
      if (imp->peek_char() != '(')
	{
	  Type* rtype = imp->read_type();
	  results->push_back(Typed_identifier(Import::import_marker, rtype,
					      imp->location()));
	}
      else
	{
	  imp->advance(1);
	  while (true)
	    {
	      Type* rtype = imp->read_type();
	      results->push_back(Typed_identifier(Import::import_marker,
						  rtype, imp->location()));
	      if (imp->peek_char() != ',')
		break;
	      imp->require_c_string(", ");
	    }
	  imp->require_c_string(")");
	}
    }

  Function_type* ret = Type::make_function_type(NULL, parameters, results,
						imp->location());
  if (is_varargs)
    ret->set_is_varargs();
  return ret;
}

// Make a copy of a function type without a receiver.

Function_type*
Function_type::copy_without_receiver() const
{
  gcc_assert(this->is_method());
  Function_type *ret = Type::make_function_type(NULL, this->parameters_,
						this->results_,
						this->location_);
  if (this->is_varargs())
    ret->set_is_varargs();
  if (this->is_builtin())
    ret->set_is_builtin();
  return ret;
}

// Make a copy of a function type with a receiver.

Function_type*
Function_type::copy_with_receiver(Type* receiver_type) const
{
  gcc_assert(!this->is_method());
  Typed_identifier* receiver = new Typed_identifier("", receiver_type,
						    this->location_);
  return Type::make_function_type(receiver, this->parameters_,
				  this->results_, this->location_);
}

// Make a function type.

Function_type*
Type::make_function_type(Typed_identifier* receiver,
			 Typed_identifier_list* parameters,
			 Typed_identifier_list* results,
			 source_location location)
{
  return new Function_type(receiver, parameters, results, location);
}

// Class Pointer_type.

// Traversal.

int
Pointer_type::do_traverse(Traverse* traverse)
{
  return Type::traverse(this->to_type_, traverse);
}

// Hash code.

unsigned int
Pointer_type::do_hash_for_method(Gogo* gogo) const
{
  return this->to_type_->hash_for_method(gogo) << 4;
}

// The tree for a pointer type.

tree
Pointer_type::do_get_tree(Gogo* gogo)
{
  return build_pointer_type(this->to_type_->get_tree(gogo));
}

// Initialize a pointer type.

tree
Pointer_type::do_get_init_tree(Gogo*, tree type_tree, bool is_clear)
{
  if (is_clear)
    return NULL;
  return fold_convert(type_tree, null_pointer_node);
}

// The type of a pointer type descriptor.

Type*
Pointer_type::make_pointer_type_descriptor_type()
{
  static Type* ret;
  if (ret == NULL)
    {
      Type* tdt = Type::make_type_descriptor_type();
      Type* ptdt = Type::make_type_descriptor_ptr_type();

      Struct_type* s = Type::make_builtin_struct_type(2,
						      "", tdt,
						      "elem", ptdt);

      ret = Type::make_builtin_named_type("PtrType", s);
    }

  return ret;
}

// The type descriptor for a pointer type.

Expression*
Pointer_type::do_type_descriptor(Gogo* gogo, Named_type* name)
{
  if (this->is_unsafe_pointer_type())
    {
      gcc_assert(name != NULL);
      return this->plain_type_descriptor(gogo,
					 RUNTIME_TYPE_KIND_UNSAFE_POINTER,
					 name);
    }
  else
    {
      source_location bloc = BUILTINS_LOCATION;

      const Methods* methods;
      Type* deref = this->points_to();
      if (deref->named_type() != NULL)
	methods = deref->named_type()->methods();
      else if (deref->struct_type() != NULL)
	methods = deref->struct_type()->methods();
      else
	methods = NULL;

      Type* ptr_tdt = Pointer_type::make_pointer_type_descriptor_type();

      const Struct_field_list* fields = ptr_tdt->struct_type()->fields();

      Expression_list* vals = new Expression_list();
      vals->reserve(2);

      Struct_field_list::const_iterator p = fields->begin();
      gcc_assert(p->field_name() == "commonType");
      vals->push_back(this->type_descriptor_constructor(gogo,
							RUNTIME_TYPE_KIND_PTR,
							name, methods, false));

      ++p;
      gcc_assert(p->field_name() == "elem");
      vals->push_back(Expression::make_type_descriptor(deref, bloc));

      return Expression::make_struct_composite_literal(ptr_tdt, vals, bloc);
    }
}

// Reflection string.

void
Pointer_type::do_reflection(Gogo* gogo, std::string* ret) const
{
  ret->push_back('*');
  this->append_reflection(this->to_type_, gogo, ret);
}

// Mangled name.

void
Pointer_type::do_mangled_name(Gogo* gogo, std::string* ret) const
{
  ret->push_back('p');
  this->append_mangled_name(this->to_type_, gogo, ret);
}

// Export.

void
Pointer_type::do_export(Export* exp) const
{
  exp->write_c_string("*");
  if (this->is_unsafe_pointer_type())
    exp->write_c_string("any");
  else
    exp->write_type(this->to_type_);
}

// Import.

Pointer_type*
Pointer_type::do_import(Import* imp)
{
  imp->require_c_string("*");
  if (imp->match_c_string("any"))
    {
      imp->advance(3);
      return Type::make_pointer_type(Type::make_void_type());
    }
  Type* to = imp->read_type();
  return Type::make_pointer_type(to);
}

// Make a pointer type.

Pointer_type*
Type::make_pointer_type(Type* to_type)
{
  typedef Unordered_map(Type*, Pointer_type*) Hashtable;
  static Hashtable pointer_types;
  Hashtable::const_iterator p = pointer_types.find(to_type);
  if (p != pointer_types.end())
    return p->second;
  Pointer_type* ret = new Pointer_type(to_type);
  pointer_types[to_type] = ret;
  return ret;
}

// The nil type.  We use a special type for nil because it is not the
// same as any other type.  In C term nil has type void*, but there is
// no such type in Go.

class Nil_type : public Type
{
 public:
  Nil_type()
    : Type(TYPE_NIL)
  { }

 protected:
  tree
  do_get_tree(Gogo*)
  { return ptr_type_node; }

  tree
  do_get_init_tree(Gogo*, tree type_tree, bool is_clear)
  { return is_clear ? NULL : fold_convert(type_tree, null_pointer_node); }

  Expression*
  do_type_descriptor(Gogo*, Named_type*)
  { gcc_unreachable(); }

  void
  do_reflection(Gogo*, std::string*) const
  { gcc_unreachable(); }

  void
  do_mangled_name(Gogo*, std::string* ret) const
  { ret->push_back('n'); }
};

// Make the nil type.

Type*
Type::make_nil_type()
{
  static Nil_type singleton_nil_type;
  return &singleton_nil_type;
}

// The type of a function call which returns multiple values.  This is
// really a struct, but we don't want to confuse a function call which
// returns a struct with a function call which returns multiple
// values.

class Call_multiple_result_type : public Type
{
 public:
  Call_multiple_result_type(Call_expression* call)
    : Type(TYPE_CALL_MULTIPLE_RESULT),
      call_(call)
  { }

 protected:
  bool
  do_has_pointer() const
  {
    gcc_assert(saw_errors());
    return false;
  }

  tree
  do_get_tree(Gogo*);

  tree
  do_get_init_tree(Gogo*, tree, bool)
  {
    gcc_assert(saw_errors());
    return error_mark_node;
  }

  Expression*
  do_type_descriptor(Gogo*, Named_type*)
  {
    gcc_assert(saw_errors());
    return Expression::make_error(UNKNOWN_LOCATION);
  }

  void
  do_reflection(Gogo*, std::string*) const
  { gcc_assert(saw_errors()); }

  void
  do_mangled_name(Gogo*, std::string*) const
  { gcc_assert(saw_errors()); }

 private:
  // The expression being called.
  Call_expression* call_;
};

// Return the tree for a call result.

tree
Call_multiple_result_type::do_get_tree(Gogo* gogo)
{
  Function_type* fntype = this->call_->get_function_type();
  gcc_assert(fntype != NULL);
  const Typed_identifier_list* results = fntype->results();
  gcc_assert(results != NULL && results->size() > 1);

  Struct_field_list* sfl = new Struct_field_list;
  for (Typed_identifier_list::const_iterator p = results->begin();
       p != results->end();
       ++p)
    {
      const std::string name = ((p->name().empty()
				 || p->name() == Import::import_marker)
				? "UNNAMED"
				: p->name());
      sfl->push_back(Struct_field(Typed_identifier(name, p->type(),
						   this->call_->location())));
    }
  return Type::make_struct_type(sfl, this->call_->location())->get_tree(gogo);
}

// Make a call result type.

Type*
Type::make_call_multiple_result_type(Call_expression* call)
{
  return new Call_multiple_result_type(call);
}

// Class Struct_field.

// Get the name of a field.

const std::string&
Struct_field::field_name() const
{
  const std::string& name(this->typed_identifier_.name());
  if (!name.empty())
    return name;
  else
    {
      // This is called during parsing, before anything is lowered, so
      // we have to be pretty careful to avoid dereferencing an
      // unknown type name.
      Type* t = this->typed_identifier_.type();
      Type* dt = t;
      if (t->classification() == Type::TYPE_POINTER)
	{
	  // Very ugly.
	  Pointer_type* ptype = static_cast<Pointer_type*>(t);
	  dt = ptype->points_to();
	}
      if (dt->forward_declaration_type() != NULL)
	return dt->forward_declaration_type()->name();
      else if (dt->named_type() != NULL)
	return dt->named_type()->name();
      else if (t->is_error_type() || dt->is_error_type())
	{
	  static const std::string error_string = "*error*";
	  return error_string;
	}
      else
	{
	  // Avoid crashing in the erroneous case where T is named but
	  // DT is not.
	  gcc_assert(t != dt);
	  if (t->forward_declaration_type() != NULL)
	    return t->forward_declaration_type()->name();
	  else if (t->named_type() != NULL)
	    return t->named_type()->name();
	  else
	    gcc_unreachable();
	}
    }
}

// Class Struct_type.

// Traversal.

int
Struct_type::do_traverse(Traverse* traverse)
{
  Struct_field_list* fields = this->fields_;
  if (fields != NULL)
    {
      for (Struct_field_list::iterator p = fields->begin();
	   p != fields->end();
	   ++p)
	{
	  if (Type::traverse(p->type(), traverse) == TRAVERSE_EXIT)
	    return TRAVERSE_EXIT;
	}
    }
  return TRAVERSE_CONTINUE;
}

// Verify that the struct type is complete and valid.

bool
Struct_type::do_verify()
{
  Struct_field_list* fields = this->fields_;
  if (fields == NULL)
    return true;
  bool ret = true;
  for (Struct_field_list::iterator p = fields->begin();
       p != fields->end();
       ++p)
    {
      Type* t = p->type();
      if (t->is_undefined())
	{
	  error_at(p->location(), "struct field type is incomplete");
	  p->set_type(Type::make_error_type());
	  ret = false;
	}
      else if (p->is_anonymous())
	{
	  if (t->named_type() != NULL && t->points_to() != NULL)
	    {
	      error_at(p->location(), "embedded type may not be a pointer");
	      p->set_type(Type::make_error_type());
	      return false;
	    }
	}
    }
  return ret;
}

// Whether this contains a pointer.

bool
Struct_type::do_has_pointer() const
{
  const Struct_field_list* fields = this->fields();
  if (fields == NULL)
    return false;
  for (Struct_field_list::const_iterator p = fields->begin();
       p != fields->end();
       ++p)
    {
      if (p->type()->has_pointer())
	return true;
    }
  return false;
}

// Whether this type is identical to T.

bool
Struct_type::is_identical(const Struct_type* t,
			  bool errors_are_identical) const
{
  const Struct_field_list* fields1 = this->fields();
  const Struct_field_list* fields2 = t->fields();
  if (fields1 == NULL || fields2 == NULL)
    return fields1 == fields2;
  Struct_field_list::const_iterator pf2 = fields2->begin();
  for (Struct_field_list::const_iterator pf1 = fields1->begin();
       pf1 != fields1->end();
       ++pf1, ++pf2)
    {
      if (pf2 == fields2->end())
	return false;
      if (pf1->field_name() != pf2->field_name())
	return false;
      if (pf1->is_anonymous() != pf2->is_anonymous()
	  || !Type::are_identical(pf1->type(), pf2->type(),
				  errors_are_identical, NULL))
	return false;
      if (!pf1->has_tag())
	{
	  if (pf2->has_tag())
	    return false;
	}
      else
	{
	  if (!pf2->has_tag())
	    return false;
	  if (pf1->tag() != pf2->tag())
	    return false;
	}
    }
  if (pf2 != fields2->end())
    return false;
  return true;
}

// Whether this struct type has any hidden fields.

bool
Struct_type::struct_has_hidden_fields(const Named_type* within,
				      std::string* reason) const
{
  const Struct_field_list* fields = this->fields();
  if (fields == NULL)
    return false;
  const Package* within_package = (within == NULL
				   ? NULL
				   : within->named_object()->package());
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf)
    {
      if (within_package != NULL
	  && !pf->is_anonymous()
	  && Gogo::is_hidden_name(pf->field_name()))
	{
	  if (reason != NULL)
	    {
	      std::string within_name = within->named_object()->message_name();
	      std::string name = Gogo::message_name(pf->field_name());
	      size_t bufsize = 200 + within_name.length() + name.length();
	      char* buf = new char[bufsize];
	      snprintf(buf, bufsize,
		       _("implicit assignment of %s%s%s hidden field %s%s%s"),
		       open_quote, within_name.c_str(), close_quote,
		       open_quote, name.c_str(), close_quote);
	      reason->assign(buf);
	      delete[] buf;
	    }
	  return true;
	}

      if (pf->type()->has_hidden_fields(within, reason))
	return true;
    }

  return false;
}

// Hash code.

unsigned int
Struct_type::do_hash_for_method(Gogo* gogo) const
{
  unsigned int ret = 0;
  if (this->fields() != NULL)
    {
      for (Struct_field_list::const_iterator pf = this->fields()->begin();
	   pf != this->fields()->end();
	   ++pf)
	ret = (ret << 1) + pf->type()->hash_for_method(gogo);
    }
  return ret <<= 2;
}

// Find the local field NAME.

const Struct_field*
Struct_type::find_local_field(const std::string& name,
			      unsigned int *pindex) const
{
  const Struct_field_list* fields = this->fields_;
  if (fields == NULL)
    return NULL;
  unsigned int i = 0;
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf, ++i)
    {
      if (pf->field_name() == name)
	{
	  if (pindex != NULL)
	    *pindex = i;
	  return &*pf;
	}
    }
  return NULL;
}

// Return an expression for field NAME in STRUCT_EXPR, or NULL.

Field_reference_expression*
Struct_type::field_reference(Expression* struct_expr, const std::string& name,
			     source_location location) const
{
  unsigned int depth;
  return this->field_reference_depth(struct_expr, name, location, NULL,
				     &depth);
}

// Return an expression for a field, along with the depth at which it
// was found.

Field_reference_expression*
Struct_type::field_reference_depth(Expression* struct_expr,
				   const std::string& name,
				   source_location location,
				   Saw_named_type* saw,
				   unsigned int* depth) const
{
  const Struct_field_list* fields = this->fields_;
  if (fields == NULL)
    return NULL;

  // Look for a field with this name.
  unsigned int i = 0;
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf, ++i)
    {
      if (pf->field_name() == name)
	{
	  *depth = 0;
	  return Expression::make_field_reference(struct_expr, i, location);
	}
    }

  // Look for an anonymous field which contains a field with this
  // name.
  unsigned int found_depth = 0;
  Field_reference_expression* ret = NULL;
  i = 0;
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf, ++i)
    {
      if (!pf->is_anonymous())
	continue;

      Struct_type* st = pf->type()->deref()->struct_type();
      if (st == NULL)
	continue;

      Saw_named_type* hold_saw = saw;
      Saw_named_type saw_here;
      Named_type* nt = pf->type()->named_type();
      if (nt == NULL)
	nt = pf->type()->deref()->named_type();
      if (nt != NULL)
	{
	  Saw_named_type* q;
	  for (q = saw; q != NULL; q = q->next)
	    {
	      if (q->nt == nt)
		{
		  // If this is an error, it will be reported
		  // elsewhere.
		  break;
		}
	    }
	  if (q != NULL)
	    continue;
	  saw_here.next = saw;
	  saw_here.nt = nt;
	  saw = &saw_here;
	}

      // Look for a reference using a NULL struct expression.  If we
      // find one, fill in the struct expression with a reference to
      // this field.
      unsigned int subdepth;
      Field_reference_expression* sub = st->field_reference_depth(NULL, name,
								  location,
								  saw,
								  &subdepth);

      saw = hold_saw;

      if (sub == NULL)
	continue;

      if (ret == NULL || subdepth < found_depth)
	{
	  if (ret != NULL)
	    delete ret;
	  ret = sub;
	  found_depth = subdepth;
	  Expression* here = Expression::make_field_reference(struct_expr, i,
							      location);
	  if (pf->type()->points_to() != NULL)
	    here = Expression::make_unary(OPERATOR_MULT, here, location);
	  while (sub->expr() != NULL)
	    {
	      sub = sub->expr()->deref()->field_reference_expression();
	      gcc_assert(sub != NULL);
	    }
	  sub->set_struct_expression(here);
	}
      else if (subdepth > found_depth)
	delete sub;
      else
	{
	  // We do not handle ambiguity here--it should be handled by
	  // Type::bind_field_or_method.
	  delete sub;
	  found_depth = 0;
	  ret = NULL;
	}
    }

  if (ret != NULL)
    *depth = found_depth + 1;

  return ret;
}

// Return the total number of fields, including embedded fields.

unsigned int
Struct_type::total_field_count() const
{
  if (this->fields_ == NULL)
    return 0;
  unsigned int ret = 0;
  for (Struct_field_list::const_iterator pf = this->fields_->begin();
       pf != this->fields_->end();
       ++pf)
    {
      if (!pf->is_anonymous() || pf->type()->deref()->struct_type() == NULL)
	++ret;
      else
	ret += pf->type()->struct_type()->total_field_count();
    }
  return ret;
}

// Return whether NAME is an unexported field, for better error reporting.

bool
Struct_type::is_unexported_local_field(Gogo* gogo,
				       const std::string& name) const
{
  const Struct_field_list* fields = this->fields_;
  if (fields != NULL)
    {
      for (Struct_field_list::const_iterator pf = fields->begin();
	   pf != fields->end();
	   ++pf)
	{
	  const std::string& field_name(pf->field_name());
	  if (Gogo::is_hidden_name(field_name)
	      && name == Gogo::unpack_hidden_name(field_name)
	      && gogo->pack_hidden_name(name, false) != field_name)
	    return true;
	}
    }
  return false;
}

// Finalize the methods of an unnamed struct.

void
Struct_type::finalize_methods(Gogo* gogo)
{
  if (this->all_methods_ != NULL)
    return;
  Type::finalize_methods(gogo, this, this->location_, &this->all_methods_);
}

// Return the method NAME, or NULL if there isn't one or if it is
// ambiguous.  Set *IS_AMBIGUOUS if the method exists but is
// ambiguous.

Method*
Struct_type::method_function(const std::string& name, bool* is_ambiguous) const
{
  return Type::method_function(this->all_methods_, name, is_ambiguous);
}

// Get the tree for a struct type.

tree
Struct_type::do_get_tree(Gogo* gogo)
{
  tree type = make_node(RECORD_TYPE);
  return this->fill_in_tree(gogo, type);
}

// Fill in the fields for a struct type.

tree
Struct_type::fill_in_tree(Gogo* gogo, tree type)
{
  tree field_trees = NULL_TREE;
  tree* pp = &field_trees;
  bool has_pointer = false;
  for (Struct_field_list::const_iterator p = this->fields_->begin();
       p != this->fields_->end();
       ++p)
    {
      std::string name = Gogo::unpack_hidden_name(p->field_name());
      tree name_tree = get_identifier_with_length(name.data(), name.length());

      // Don't follow pointers yet, so that we don't get confused by a
      // pointer to an array of this struct type.
      tree field_type_tree;
      if (p->type()->points_to() != NULL || p->type()->function_type() != NULL)
	{
	  field_type_tree = ptr_type_node;
	  has_pointer = true;
	}
      else
	{
	  field_type_tree = p->type()->get_tree(gogo);
	  if (field_type_tree == error_mark_node)
	    return error_mark_node;
	}

      tree field = build_decl(p->location(), FIELD_DECL, name_tree,
			      field_type_tree);
      DECL_CONTEXT(field) = type;
      *pp = field;
      pp = &DECL_CHAIN(field);
    }

  TYPE_FIELDS(type) = field_trees;

  layout_type(type);

  if (has_pointer)
    {
      tree field = field_trees;
      for (Struct_field_list::const_iterator p = this->fields_->begin();
	   p != this->fields_->end();
	   ++p, field = DECL_CHAIN(field))
	{
	  if (p->type()->points_to() != NULL
	      || p->type()->function_type() != NULL)
	    TREE_TYPE(field) = p->type()->get_tree(gogo);
	}
    }

  return type;
}

// Make sure that all structs which must be converted to the backend
// representation before this one are in fact converted.

void
Struct_type::convert_prerequisites(Gogo* gogo)
{
  for (std::vector<Named_type*>::const_iterator p
	 = this->prerequisites_.begin();
       p != this->prerequisites_.end();
       ++p)
    (*p)->get_tree(gogo);
}

// Initialize struct fields.

tree
Struct_type::do_get_init_tree(Gogo* gogo, tree type_tree, bool is_clear)
{
  if (this->fields_ == NULL || this->fields_->empty())
    {
      if (is_clear)
	return NULL;
      else
	{
	  tree ret = build_constructor(type_tree,
				       VEC_alloc(constructor_elt, gc, 0));
	  TREE_CONSTANT(ret) = 1;
	  return ret;
	}
    }

  bool is_constant = true;
  bool any_fields_set = false;
  VEC(constructor_elt,gc)* init = VEC_alloc(constructor_elt, gc,
					    this->fields_->size());

  tree field = TYPE_FIELDS(type_tree);
  for (Struct_field_list::const_iterator p = this->fields_->begin();
       p != this->fields_->end();
       ++p, field = DECL_CHAIN(field))
    {
      tree value = p->type()->get_init_tree(gogo, is_clear);
      if (value == error_mark_node)
	return error_mark_node;
      gcc_assert(field != NULL_TREE);
      if (value != NULL)
	{
	  constructor_elt* elt = VEC_quick_push(constructor_elt, init, NULL);
	  elt->index = field;
	  elt->value = value;
	  any_fields_set = true;
	  if (!TREE_CONSTANT(value))
	    is_constant = false;
	}
    }
  gcc_assert(field == NULL_TREE);

  if (!any_fields_set)
    {
      gcc_assert(is_clear);
      VEC_free(constructor_elt, gc, init);
      return NULL;
    }

  tree ret = build_constructor(type_tree, init);
  if (is_constant)
    TREE_CONSTANT(ret) = 1;
  return ret;
}

// The type of a struct type descriptor.

Type*
Struct_type::make_struct_type_descriptor_type()
{
  static Type* ret;
  if (ret == NULL)
    {
      Type* tdt = Type::make_type_descriptor_type();
      Type* ptdt = Type::make_type_descriptor_ptr_type();

      Type* uintptr_type = Type::lookup_integer_type("uintptr");
      Type* string_type = Type::lookup_string_type();
      Type* pointer_string_type = Type::make_pointer_type(string_type);

      Struct_type* sf =
	Type::make_builtin_struct_type(5,
				       "name", pointer_string_type,
				       "pkgPath", pointer_string_type,
				       "typ", ptdt,
				       "tag", pointer_string_type,
				       "offset", uintptr_type);
      Type* nsf = Type::make_builtin_named_type("structField", sf);

      Type* slice_type = Type::make_array_type(nsf, NULL);

      Struct_type* s = Type::make_builtin_struct_type(2,
						      "", tdt,
						      "fields", slice_type);

      ret = Type::make_builtin_named_type("StructType", s);
    }

  return ret;
}

// Build a type descriptor for a struct type.

Expression*
Struct_type::do_type_descriptor(Gogo* gogo, Named_type* name)
{
  source_location bloc = BUILTINS_LOCATION;

  Type* stdt = Struct_type::make_struct_type_descriptor_type();

  const Struct_field_list* fields = stdt->struct_type()->fields();

  Expression_list* vals = new Expression_list();
  vals->reserve(2);

  const Methods* methods = this->methods();
  // A named struct should not have methods--the methods should attach
  // to the named type.
  gcc_assert(methods == NULL || name == NULL);

  Struct_field_list::const_iterator ps = fields->begin();
  gcc_assert(ps->field_name() == "commonType");
  vals->push_back(this->type_descriptor_constructor(gogo,
						    RUNTIME_TYPE_KIND_STRUCT,
						    name, methods, true));

  ++ps;
  gcc_assert(ps->field_name() == "fields");

  Expression_list* elements = new Expression_list();
  elements->reserve(this->fields_->size());
  Type* element_type = ps->type()->array_type()->element_type();
  for (Struct_field_list::const_iterator pf = this->fields_->begin();
       pf != this->fields_->end();
       ++pf)
    {
      const Struct_field_list* f = element_type->struct_type()->fields();

      Expression_list* fvals = new Expression_list();
      fvals->reserve(5);

      Struct_field_list::const_iterator q = f->begin();
      gcc_assert(q->field_name() == "name");
      if (pf->is_anonymous())
	fvals->push_back(Expression::make_nil(bloc));
      else
	{
	  std::string n = Gogo::unpack_hidden_name(pf->field_name());
	  Expression* s = Expression::make_string(n, bloc);
	  fvals->push_back(Expression::make_unary(OPERATOR_AND, s, bloc));
	}

      ++q;
      gcc_assert(q->field_name() == "pkgPath");
      if (!Gogo::is_hidden_name(pf->field_name()))
	fvals->push_back(Expression::make_nil(bloc));
      else
	{
	  std::string n = Gogo::hidden_name_prefix(pf->field_name());
	  Expression* s = Expression::make_string(n, bloc);
	  fvals->push_back(Expression::make_unary(OPERATOR_AND, s, bloc));
	}

      ++q;
      gcc_assert(q->field_name() == "typ");
      fvals->push_back(Expression::make_type_descriptor(pf->type(), bloc));

      ++q;
      gcc_assert(q->field_name() == "tag");
      if (!pf->has_tag())
	fvals->push_back(Expression::make_nil(bloc));
      else
	{
	  Expression* s = Expression::make_string(pf->tag(), bloc);
	  fvals->push_back(Expression::make_unary(OPERATOR_AND, s, bloc));
	}

      ++q;
      gcc_assert(q->field_name() == "offset");
      fvals->push_back(Expression::make_struct_field_offset(this, &*pf));

      Expression* v = Expression::make_struct_composite_literal(element_type,
								fvals, bloc);
      elements->push_back(v);
    }

  vals->push_back(Expression::make_slice_composite_literal(ps->type(),
							   elements, bloc));

  return Expression::make_struct_composite_literal(stdt, vals, bloc);
}

// Reflection string.

void
Struct_type::do_reflection(Gogo* gogo, std::string* ret) const
{
  ret->append("struct { ");

  for (Struct_field_list::const_iterator p = this->fields_->begin();
       p != this->fields_->end();
       ++p)
    {
      if (p != this->fields_->begin())
	ret->append("; ");
      if (p->is_anonymous())
	ret->push_back('?');
      else
	ret->append(Gogo::unpack_hidden_name(p->field_name()));
      ret->push_back(' ');
      this->append_reflection(p->type(), gogo, ret);

      if (p->has_tag())
	{
	  const std::string& tag(p->tag());
	  ret->append(" \"");
	  for (std::string::const_iterator p = tag.begin();
	       p != tag.end();
	       ++p)
	    {
	      if (*p == '\0')
		ret->append("\\x00");
	      else if (*p == '\n')
		ret->append("\\n");
	      else if (*p == '\t')
		ret->append("\\t");
	      else if (*p == '"')
		ret->append("\\\"");
	      else if (*p == '\\')
		ret->append("\\\\");
	      else
		ret->push_back(*p);
	    }
	  ret->push_back('"');
	}
    }

  ret->append(" }");
}

// Mangled name.

void
Struct_type::do_mangled_name(Gogo* gogo, std::string* ret) const
{
  ret->push_back('S');

  const Struct_field_list* fields = this->fields_;
  if (fields != NULL)
    {
      for (Struct_field_list::const_iterator p = fields->begin();
	   p != fields->end();
	   ++p)
	{
	  if (p->is_anonymous())
	    ret->append("0_");
	  else
	    {
	      std::string n = Gogo::unpack_hidden_name(p->field_name());
	      char buf[20];
	      snprintf(buf, sizeof buf, "%u_",
		       static_cast<unsigned int>(n.length()));
	      ret->append(buf);
	      ret->append(n);
	    }
	  this->append_mangled_name(p->type(), gogo, ret);
	  if (p->has_tag())
	    {
	      const std::string& tag(p->tag());
	      std::string out;
	      for (std::string::const_iterator p = tag.begin();
		   p != tag.end();
		   ++p)
		{
		  if (ISALNUM(*p) || *p == '_')
		    out.push_back(*p);
		  else
		    {
		      char buf[20];
		      snprintf(buf, sizeof buf, ".%x.",
			       static_cast<unsigned int>(*p));
		      out.append(buf);
		    }
		}
	      char buf[20];
	      snprintf(buf, sizeof buf, "T%u_",
		       static_cast<unsigned int>(out.length()));
	      ret->append(buf);
	      ret->append(out);
	    }
	}
    }

  ret->push_back('e');
}

// Export.

void
Struct_type::do_export(Export* exp) const
{
  exp->write_c_string("struct { ");
  const Struct_field_list* fields = this->fields_;
  gcc_assert(fields != NULL);
  for (Struct_field_list::const_iterator p = fields->begin();
       p != fields->end();
       ++p)
    {
      if (p->is_anonymous())
	exp->write_string("? ");
      else
	{
	  exp->write_string(p->field_name());
	  exp->write_c_string(" ");
	}
      exp->write_type(p->type());

      if (p->has_tag())
	{
	  exp->write_c_string(" ");
	  Expression* expr = Expression::make_string(p->tag(),
						     BUILTINS_LOCATION);
	  expr->export_expression(exp);
	  delete expr;
	}

      exp->write_c_string("; ");
    }
  exp->write_c_string("}");
}

// Import.

Struct_type*
Struct_type::do_import(Import* imp)
{
  imp->require_c_string("struct { ");
  Struct_field_list* fields = new Struct_field_list;
  if (imp->peek_char() != '}')
    {
      while (true)
	{
	  std::string name;
	  if (imp->match_c_string("? "))
	    imp->advance(2);
	  else
	    {
	      name = imp->read_identifier();
	      imp->require_c_string(" ");
	    }
	  Type* ftype = imp->read_type();

	  Struct_field sf(Typed_identifier(name, ftype, imp->location()));

	  if (imp->peek_char() == ' ')
	    {
	      imp->advance(1);
	      Expression* expr = Expression::import_expression(imp);
	      String_expression* sexpr = expr->string_expression();
	      gcc_assert(sexpr != NULL);
	      sf.set_tag(sexpr->val());
	      delete sexpr;
	    }

	  imp->require_c_string("; ");
	  fields->push_back(sf);
	  if (imp->peek_char() == '}')
	    break;
	}
    }
  imp->require_c_string("}");

  return Type::make_struct_type(fields, imp->location());
}

// Make a struct type.

Struct_type*
Type::make_struct_type(Struct_field_list* fields,
		       source_location location)
{
  return new Struct_type(fields, location);
}

// Class Array_type.

// Whether two array types are identical.

bool
Array_type::is_identical(const Array_type* t, bool errors_are_identical) const
{
  if (!Type::are_identical(this->element_type(), t->element_type(),
			   errors_are_identical, NULL))
    return false;

  Expression* l1 = this->length();
  Expression* l2 = t->length();

  // Slices of the same element type are identical.
  if (l1 == NULL && l2 == NULL)
    return true;

  // Arrays of the same element type are identical if they have the
  // same length.
  if (l1 != NULL && l2 != NULL)
    {
      if (l1 == l2)
	return true;

      // Try to determine the lengths.  If we can't, assume the arrays
      // are not identical.
      bool ret = false;
      mpz_t v1;
      mpz_init(v1);
      Type* type1;
      mpz_t v2;
      mpz_init(v2);
      Type* type2;
      if (l1->integer_constant_value(true, v1, &type1)
	  && l2->integer_constant_value(true, v2, &type2))
	ret = mpz_cmp(v1, v2) == 0;
      mpz_clear(v1);
      mpz_clear(v2);
      return ret;
    }

  // Otherwise the arrays are not identical.
  return false;
}

// Traversal.

int
Array_type::do_traverse(Traverse* traverse)
{
  if (Type::traverse(this->element_type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (this->length_ != NULL
      && Expression::traverse(&this->length_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Check that the length is valid.

bool
Array_type::verify_length()
{
  if (this->length_ == NULL)
    return true;

  Type_context context(Type::lookup_integer_type("int"), false);
  this->length_->determine_type(&context);

  if (!this->length_->is_constant())
    {
      error_at(this->length_->location(), "array bound is not constant");
      return false;
    }

  mpz_t val;
  mpz_init(val);
  Type* vt;
  if (!this->length_->integer_constant_value(true, val, &vt))
    {
      mpfr_t fval;
      mpfr_init(fval);
      if (!this->length_->float_constant_value(fval, &vt))
	{
	  if (this->length_->type()->integer_type() != NULL
	      || this->length_->type()->float_type() != NULL)
	    error_at(this->length_->location(),
		     "array bound is not constant");
	  else
	    error_at(this->length_->location(),
		     "array bound is not numeric");
	  mpfr_clear(fval);
	  mpz_clear(val);
	  return false;
	}
      if (!mpfr_integer_p(fval))
	{
	  error_at(this->length_->location(),
		   "array bound truncated to integer");
	  mpfr_clear(fval);
	  mpz_clear(val);
	  return false;
	}
      mpz_init(val);
      mpfr_get_z(val, fval, GMP_RNDN);
      mpfr_clear(fval);
    }

  if (mpz_sgn(val) < 0)
    {
      error_at(this->length_->location(), "negative array bound");
      mpz_clear(val);
      return false;
    }

  Type* int_type = Type::lookup_integer_type("int");
  int tbits = int_type->integer_type()->bits();
  int vbits = mpz_sizeinbase(val, 2);
  if (vbits + 1 > tbits)
    {
      error_at(this->length_->location(), "array bound overflows");
      mpz_clear(val);
      return false;
    }

  mpz_clear(val);

  return true;
}

// Verify the type.

bool
Array_type::do_verify()
{
  if (!this->verify_length())
    {
      this->length_ = Expression::make_error(this->length_->location());
      return false;
    }
  return true;
}

// Array type hash code.

unsigned int
Array_type::do_hash_for_method(Gogo* gogo) const
{
  // There is no very convenient way to get a hash code for the
  // length.
  return this->element_type_->hash_for_method(gogo) + 1;
}

// See if the expression passed to make is suitable.  The first
// argument is required, and gives the length.  An optional second
// argument is permitted for the capacity.

bool
Array_type::do_check_make_expression(Expression_list* args,
				     source_location location)
{
  gcc_assert(this->length_ == NULL);
  if (args == NULL || args->empty())
    {
      error_at(location, "length required when allocating a slice");
      return false;
    }
  else if (args->size() > 2)
    {
      error_at(location, "too many expressions passed to make");
      return false;
    }
  else
    {
      if (!Type::check_int_value(args->front(),
				 _("bad length when making slice"), location))
	return false;

      if (args->size() > 1)
	{
	  if (!Type::check_int_value(args->back(),
				     _("bad capacity when making slice"),
				     location))
	    return false;
	}

      return true;
    }
}

// Get a tree for the length of a fixed array.  The length may be
// computed using a function call, so we must only evaluate it once.

tree
Array_type::get_length_tree(Gogo* gogo)
{
  gcc_assert(this->length_ != NULL);
  if (this->length_tree_ == NULL_TREE)
    {
      mpz_t val;
      mpz_init(val);
      Type* t;
      if (this->length_->integer_constant_value(true, val, &t))
	{
	  if (t == NULL)
	    t = Type::lookup_integer_type("int");
	  else if (t->is_abstract())
	    t = t->make_non_abstract_type();
	  tree tt = t->get_tree(gogo);
	  this->length_tree_ = Expression::integer_constant_tree(val, tt);
	  mpz_clear(val);
	}
      else
	{
	  mpz_clear(val);

	  // Make up a translation context for the array length
	  // expression.  FIXME: This won't work in general.
	  Translate_context context(gogo, NULL, NULL, NULL_TREE);
	  tree len = this->length_->get_tree(&context);
	  if (len != error_mark_node)
	    {
	      len = convert_to_integer(integer_type_node, len);
	      len = save_expr(len);
	    }
	  this->length_tree_ = len;
	}
    }
  return this->length_tree_;
}

// Get a tree for the type of this array.  A fixed array is simply
// represented as ARRAY_TYPE with the appropriate index--i.e., it is
// just like an array in C.  An open array is a struct with three
// fields: a data pointer, the length, and the capacity.

tree
Array_type::do_get_tree(Gogo* gogo)
{
  if (this->length_ == NULL)
    {
      tree struct_type = gogo->slice_type_tree(void_type_node);
      return this->fill_in_slice_tree(gogo, struct_type);
    }
  else
    {
      tree array_type = make_node(ARRAY_TYPE);
      return this->fill_in_array_tree(gogo, array_type);
    }
}

// Fill in the fields for an array type.  This is used for named array
// types.

tree
Array_type::fill_in_array_tree(Gogo* gogo, tree array_type)
{
  gcc_assert(this->length_ != NULL);

  tree element_type_tree = this->element_type_->get_tree(gogo);
  tree length_tree = this->get_length_tree(gogo);
  if (element_type_tree == error_mark_node
      || length_tree == error_mark_node)
    return error_mark_node;

  length_tree = fold_convert(sizetype, length_tree);

  // build_index_type takes the maximum index, which is one less than
  // the length.
  tree index_type = build_index_type(fold_build2(MINUS_EXPR, sizetype,
						 length_tree,
						 size_one_node));

  TREE_TYPE(array_type) = element_type_tree;
  TYPE_DOMAIN(array_type) = index_type;
  TYPE_ADDR_SPACE(array_type) = TYPE_ADDR_SPACE(element_type_tree);
  layout_type(array_type);

  if (TYPE_STRUCTURAL_EQUALITY_P(element_type_tree)
      || TYPE_STRUCTURAL_EQUALITY_P(index_type))
    SET_TYPE_STRUCTURAL_EQUALITY(array_type);
  else if (TYPE_CANONICAL(element_type_tree) != element_type_tree
	   || TYPE_CANONICAL(index_type) != index_type)
    TYPE_CANONICAL(array_type) =
      build_array_type(TYPE_CANONICAL(element_type_tree),
		       TYPE_CANONICAL(index_type));

  return array_type;
}

// Fill in the fields for a slice type.  This is used for named slice
// types.

tree
Array_type::fill_in_slice_tree(Gogo* gogo, tree struct_type)
{
  gcc_assert(this->length_ == NULL);

  tree element_type_tree = this->element_type_->get_tree(gogo);
  if (element_type_tree == error_mark_node)
    return error_mark_node;
  tree field = TYPE_FIELDS(struct_type);
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__values") == 0);
  gcc_assert(POINTER_TYPE_P(TREE_TYPE(field))
	     && TREE_TYPE(TREE_TYPE(field)) == void_type_node);
  TREE_TYPE(field) = build_pointer_type(element_type_tree);

  return struct_type;
}

// Return an initializer for an array type.

tree
Array_type::do_get_init_tree(Gogo* gogo, tree type_tree, bool is_clear)
{
  if (this->length_ == NULL)
    {
      // Open array.

      if (is_clear)
	return NULL;

      gcc_assert(TREE_CODE(type_tree) == RECORD_TYPE);

      VEC(constructor_elt,gc)* init = VEC_alloc(constructor_elt, gc, 3);

      for (tree field = TYPE_FIELDS(type_tree);
	   field != NULL_TREE;
	   field = DECL_CHAIN(field))
	{
	  constructor_elt* elt = VEC_quick_push(constructor_elt, init,
						NULL);
	  elt->index = field;
	  elt->value = fold_convert(TREE_TYPE(field), size_zero_node);
	}

      tree ret = build_constructor(type_tree, init);
      TREE_CONSTANT(ret) = 1;
      return ret;
    }
  else
    {
      // Fixed array.

      tree value = this->element_type_->get_init_tree(gogo, is_clear);
      if (value == NULL)
	return NULL;
      if (value == error_mark_node)
	return error_mark_node;

      tree length_tree = this->get_length_tree(gogo);
      if (length_tree == error_mark_node)
	return error_mark_node;

      length_tree = fold_convert(sizetype, length_tree);
      tree range = build2(RANGE_EXPR, sizetype, size_zero_node,
			  fold_build2(MINUS_EXPR, sizetype,
				      length_tree, size_one_node));
      tree ret = build_constructor_single(type_tree, range, value);
      if (TREE_CONSTANT(value))
	TREE_CONSTANT(ret) = 1;
      return ret;
    }
}

// Handle the builtin make function for a slice.

tree
Array_type::do_make_expression_tree(Translate_context* context,
				    Expression_list* args,
				    source_location location)
{
  gcc_assert(this->length_ == NULL);

  Gogo* gogo = context->gogo();
  tree type_tree = this->get_tree(gogo);
  if (type_tree == error_mark_node)
    return error_mark_node;

  tree values_field = TYPE_FIELDS(type_tree);
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(values_field)),
		    "__values") == 0);

  tree count_field = DECL_CHAIN(values_field);
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(count_field)),
		    "__count") == 0);

  tree element_type_tree = this->element_type_->get_tree(gogo);
  if (element_type_tree == error_mark_node)
    return error_mark_node;
  tree element_size_tree = TYPE_SIZE_UNIT(element_type_tree);

  tree value = this->element_type_->get_init_tree(gogo, true);
  if (value == error_mark_node)
    return error_mark_node;

  // The first argument is the number of elements, the optional second
  // argument is the capacity.
  gcc_assert(args != NULL && args->size() >= 1 && args->size() <= 2);

  tree length_tree = args->front()->get_tree(context);
  if (length_tree == error_mark_node)
    return error_mark_node;
  if (!DECL_P(length_tree))
    length_tree = save_expr(length_tree);
  if (!INTEGRAL_TYPE_P(TREE_TYPE(length_tree)))
    length_tree = convert_to_integer(TREE_TYPE(count_field), length_tree);

  tree bad_index = Expression::check_bounds(length_tree,
					    TREE_TYPE(count_field),
					    NULL_TREE, location);

  length_tree = fold_convert_loc(location, TREE_TYPE(count_field), length_tree);
  tree capacity_tree;
  if (args->size() == 1)
    capacity_tree = length_tree;
  else
    {
      capacity_tree = args->back()->get_tree(context);
      if (capacity_tree == error_mark_node)
	return error_mark_node;
      if (!DECL_P(capacity_tree))
	capacity_tree = save_expr(capacity_tree);
      if (!INTEGRAL_TYPE_P(TREE_TYPE(capacity_tree)))
	capacity_tree = convert_to_integer(TREE_TYPE(count_field),
					   capacity_tree);

      bad_index = Expression::check_bounds(capacity_tree,
					   TREE_TYPE(count_field),
					   bad_index, location);

      tree chktype = (((TYPE_SIZE(TREE_TYPE(capacity_tree))
			> TYPE_SIZE(TREE_TYPE(length_tree)))
		       || ((TYPE_SIZE(TREE_TYPE(capacity_tree))
			    == TYPE_SIZE(TREE_TYPE(length_tree)))
			   && TYPE_UNSIGNED(TREE_TYPE(capacity_tree))))
		      ? TREE_TYPE(capacity_tree)
		      : TREE_TYPE(length_tree));
      tree chk = fold_build2_loc(location, LT_EXPR, boolean_type_node,
				 fold_convert_loc(location, chktype,
						  capacity_tree),
				 fold_convert_loc(location, chktype,
						  length_tree));
      if (bad_index == NULL_TREE)
	bad_index = chk;
      else
	bad_index = fold_build2_loc(location, TRUTH_OR_EXPR, boolean_type_node,
				    bad_index, chk);

      capacity_tree = fold_convert_loc(location, TREE_TYPE(count_field),
				       capacity_tree);
    }

  tree size_tree = fold_build2_loc(location, MULT_EXPR, sizetype,
				   element_size_tree,
				   fold_convert_loc(location, sizetype,
						    capacity_tree));

  tree chk = fold_build2_loc(location, TRUTH_AND_EXPR, boolean_type_node,
			     fold_build2_loc(location, GT_EXPR,
					     boolean_type_node,
					     fold_convert_loc(location,
							      sizetype,
							      capacity_tree),
					     size_zero_node),
			     fold_build2_loc(location, LT_EXPR,
					     boolean_type_node,
					     size_tree, element_size_tree));
  if (bad_index == NULL_TREE)
    bad_index = chk;
  else
    bad_index = fold_build2_loc(location, TRUTH_OR_EXPR, boolean_type_node,
				bad_index, chk);

  tree space = context->gogo()->allocate_memory(this->element_type_,
						size_tree, location);

  if (value != NULL_TREE)
    space = save_expr(space);

  space = fold_convert(TREE_TYPE(values_field), space);

  if (bad_index != NULL_TREE && bad_index != boolean_false_node)
    {
      tree crash = Gogo::runtime_error(RUNTIME_ERROR_MAKE_SLICE_OUT_OF_BOUNDS,
				       location);
      space = build2(COMPOUND_EXPR, TREE_TYPE(space),
		     build3(COND_EXPR, void_type_node,
			    bad_index, crash, NULL_TREE),
		     space);
    }

  tree constructor = gogo->slice_constructor(type_tree, space, length_tree,
					     capacity_tree);

  if (value == NULL_TREE)
    {
      // The array contents are zero initialized.
      return constructor;
    }

  // The elements must be initialized.

  tree max = fold_build2_loc(location, MINUS_EXPR, TREE_TYPE(count_field),
			     capacity_tree,
			     fold_convert_loc(location, TREE_TYPE(count_field),
					      integer_one_node));

  tree array_type = build_array_type(element_type_tree,
				     build_index_type(max));

  tree value_pointer = fold_convert_loc(location,
					build_pointer_type(array_type),
					space);

  tree range = build2(RANGE_EXPR, sizetype, size_zero_node, max);
  tree space_init = build_constructor_single(array_type, range, value);

  return build2(COMPOUND_EXPR, TREE_TYPE(constructor),
		build2(MODIFY_EXPR, void_type_node,
		       build_fold_indirect_ref(value_pointer),
		       space_init),
		constructor);
}

// Return a tree for a pointer to the values in ARRAY.

tree
Array_type::value_pointer_tree(Gogo*, tree array) const
{
  tree ret;
  if (this->length() != NULL)
    {
      // Fixed array.
      ret = fold_convert(build_pointer_type(TREE_TYPE(TREE_TYPE(array))),
			 build_fold_addr_expr(array));
    }
  else
    {
      // Open array.
      tree field = TYPE_FIELDS(TREE_TYPE(array));
      gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)),
			"__values") == 0);
      ret = fold_build3(COMPONENT_REF, TREE_TYPE(field), array, field,
			NULL_TREE);
    }
  if (TREE_CONSTANT(array))
    TREE_CONSTANT(ret) = 1;
  return ret;
}

// Return a tree for the length of the array ARRAY which has this
// type.

tree
Array_type::length_tree(Gogo* gogo, tree array)
{
  if (this->length_ != NULL)
    {
      if (TREE_CODE(array) == SAVE_EXPR)
	return fold_convert(integer_type_node, this->get_length_tree(gogo));
      else
	return omit_one_operand(integer_type_node,
				this->get_length_tree(gogo), array);
    }

  // This is an open array.  We need to read the length field.

  tree type = TREE_TYPE(array);
  gcc_assert(TREE_CODE(type) == RECORD_TYPE);

  tree field = DECL_CHAIN(TYPE_FIELDS(type));
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__count") == 0);

  tree ret = build3(COMPONENT_REF, TREE_TYPE(field), array, field, NULL_TREE);
  if (TREE_CONSTANT(array))
    TREE_CONSTANT(ret) = 1;
  return ret;
}

// Return a tree for the capacity of the array ARRAY which has this
// type.

tree
Array_type::capacity_tree(Gogo* gogo, tree array)
{
  if (this->length_ != NULL)
    return omit_one_operand(sizetype, this->get_length_tree(gogo), array);

  // This is an open array.  We need to read the capacity field.

  tree type = TREE_TYPE(array);
  gcc_assert(TREE_CODE(type) == RECORD_TYPE);

  tree field = DECL_CHAIN(DECL_CHAIN(TYPE_FIELDS(type)));
  gcc_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__capacity") == 0);

  return build3(COMPONENT_REF, TREE_TYPE(field), array, field, NULL_TREE);
}

// Export.

void
Array_type::do_export(Export* exp) const
{
  exp->write_c_string("[");
  if (this->length_ != NULL)
    this->length_->export_expression(exp);
  exp->write_c_string("] ");
  exp->write_type(this->element_type_);
}

// Import.

Array_type*
Array_type::do_import(Import* imp)
{
  imp->require_c_string("[");
  Expression* length;
  if (imp->peek_char() == ']')
    length = NULL;
  else
    length = Expression::import_expression(imp);
  imp->require_c_string("] ");
  Type* element_type = imp->read_type();
  return Type::make_array_type(element_type, length);
}

// The type of an array type descriptor.

Type*
Array_type::make_array_type_descriptor_type()
{
  static Type* ret;
  if (ret == NULL)
    {
      Type* tdt = Type::make_type_descriptor_type();
      Type* ptdt = Type::make_type_descriptor_ptr_type();

      Type* uintptr_type = Type::lookup_integer_type("uintptr");

      Struct_type* sf =
	Type::make_builtin_struct_type(3,
				       "", tdt,
				       "elem", ptdt,
				       "len", uintptr_type);

      ret = Type::make_builtin_named_type("ArrayType", sf);
    }

  return ret;
}

// The type of an slice type descriptor.

Type*
Array_type::make_slice_type_descriptor_type()
{
  static Type* ret;
  if (ret == NULL)
    {
      Type* tdt = Type::make_type_descriptor_type();
      Type* ptdt = Type::make_type_descriptor_ptr_type();

      Struct_type* sf =
	Type::make_builtin_struct_type(2,
				       "", tdt,
				       "elem", ptdt);

      ret = Type::make_builtin_named_type("SliceType", sf);
    }

  return ret;
}

// Build a type descriptor for an array/slice type.

Expression*
Array_type::do_type_descriptor(Gogo* gogo, Named_type* name)
{
  if (this->length_ != NULL)
    return this->array_type_descriptor(gogo, name);
  else
    return this->slice_type_descriptor(gogo, name);
}

// Build a type descriptor for an array type.

Expression*
Array_type::array_type_descriptor(Gogo* gogo, Named_type* name)
{
  source_location bloc = BUILTINS_LOCATION;

  Type* atdt = Array_type::make_array_type_descriptor_type();

  const Struct_field_list* fields = atdt->struct_type()->fields();

  Expression_list* vals = new Expression_list();
  vals->reserve(3);

  Struct_field_list::const_iterator p = fields->begin();
  gcc_assert(p->field_name() == "commonType");
  vals->push_back(this->type_descriptor_constructor(gogo,
						    RUNTIME_TYPE_KIND_ARRAY,
						    name, NULL, true));

  ++p;
  gcc_assert(p->field_name() == "elem");
  vals->push_back(Expression::make_type_descriptor(this->element_type_, bloc));

  ++p;
  gcc_assert(p->field_name() == "len");
  vals->push_back(Expression::make_cast(p->type(), this->length_, bloc));

  ++p;
  gcc_assert(p == fields->end());

  return Expression::make_struct_composite_literal(atdt, vals, bloc);
}

// Build a type descriptor for a slice type.

Expression*
Array_type::slice_type_descriptor(Gogo* gogo, Named_type* name)
{
  source_location bloc = BUILTINS_LOCATION;

  Type* stdt = Array_type::make_slice_type_descriptor_type();

  const Struct_field_list* fields = stdt->struct_type()->fields();

  Expression_list* vals = new Expression_list();
  vals->reserve(2);

  Struct_field_list::const_iterator p = fields->begin();
  gcc_assert(p->field_name() == "commonType");
  vals->push_back(this->type_descriptor_constructor(gogo,
						    RUNTIME_TYPE_KIND_SLICE,
						    name, NULL, true));

  ++p;
  gcc_assert(p->field_name() == "elem");
  vals->push_back(Expression::make_type_descriptor(this->element_type_, bloc));

  ++p;
  gcc_assert(p == fields->end());

  return Expression::make_struct_composite_literal(stdt, vals, bloc);
}

// Reflection string.

void
Array_type::do_reflection(Gogo* gogo, std::string* ret) const
{
  ret->push_back('[');
  if (this->length_ != NULL)
    {
      mpz_t val;
      mpz_init(val);
      Type* type;
      if (!this->length_->integer_constant_value(true, val, &type))
	error_at(this->length_->location(),
		 "array length must be integer constant expression");
      else if (mpz_cmp_si(val, 0) < 0)
	error_at(this->length_->location(), "array length is negative");
      else if (mpz_cmp_ui(val, mpz_get_ui(val)) != 0)
	error_at(this->length_->location(), "array length is too large");
      else
	{
	  char buf[50];
	  snprintf(buf, sizeof buf, "%lu", mpz_get_ui(val));
	  ret->append(buf);
	}
      mpz_clear(val);
    }
  ret->push_back(']');

  this->append_reflection(this->element_type_, gogo, ret);
}

// Mangled name.

void
Array_type::do_mangled_name(Gogo* gogo, std::string* ret) const
{
  ret->push_back('A');
  this->append_mangled_name(this->element_type_, gogo, ret);
  if (this->length_ != NULL)
    {
      mpz_t val;
      mpz_init(val);
      Type* type;
      if (!this->length_->integer_constant_value(true, val, &type))
	error_at(this->length_->location(),
		 "array length must be integer constant expression");
      else if (mpz_cmp_si(val, 0) < 0)
	error_at(this->length_->location(), "array length is negative");
      else if (mpz_cmp_ui(val, mpz_get_ui(val)) != 0)
	error_at(this->length_->location(), "array size is too large");
      else
	{
	  char buf[50];
	  snprintf(buf, sizeof buf, "%lu", mpz_get_ui(val));
	  ret->append(buf);
	}
      mpz_clear(val);
    }
  ret->push_back('e');
}

// Make an array type.

Array_type*
Type::make_array_type(Type* element_type, Expression* length)
{
  return new Array_type(element_type, length);
}

// Class Map_type.

// Traversal.

int
Map_type::do_traverse(Traverse* traverse)
{
  if (Type::traverse(this->key_type_, traverse) == TRAVERSE_EXIT
      || Type::traverse(this->val_type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Check that the map type is OK.

bool
Map_type::do_verify()
{
  if (this->key_type_->struct_type() != NULL
      || this->key_type_->array_type() != NULL)
    {
      error_at(this->location_, "invalid map key type");
      return false;
    }
  return true;
}

// Whether two map types are identical.

bool
Map_type::is_identical(const Map_type* t, bool errors_are_identical) const
{
  return (Type::are_identical(this->key_type(), t->key_type(),
			      errors_are_identical, NULL)
	  && Type::are_identical(this->val_type(), t->val_type(),
				 errors_are_identical, NULL));
}

// Hash code.

unsigned int
Map_type::do_hash_for_method(Gogo* gogo) const
{
  return (this->key_type_->hash_for_method(gogo)
	  + this->val_type_->hash_for_method(gogo)
	  + 2);
}

// Check that a call to the builtin make function is valid.  For a map
// the optional argument is the number of spaces to preallocate for
// values.

bool
Map_type::do_check_make_expression(Expression_list* args,
				   source_location location)
{
  if (args != NULL && !args->empty())
    {
      if (!Type::check_int_value(args->front(), _("bad size when making map"),
				 location))
	return false;
      else if (args->size() > 1)
	{
	  error_at(location, "too many arguments when making map");
	  return false;
	}
    }
  return true;
}

// Get a tree for a map type.  A map type is represented as a pointer
// to a struct.  The struct is __go_map in libgo/map.h.

tree
Map_type::do_get_tree(Gogo* gogo)
{
  static tree type_tree;
  if (type_tree == NULL_TREE)
    {
      tree struct_type = make_node(RECORD_TYPE);

      tree map_descriptor_type = gogo->map_descriptor_type();
      tree const_map_descriptor_type =
	build_qualified_type(map_descriptor_type, TYPE_QUAL_CONST);
      tree name = get_identifier("__descriptor");
      tree field = build_decl(BUILTINS_LOCATION, FIELD_DECL, name,
			      build_pointer_type(const_map_descriptor_type));
      DECL_CONTEXT(field) = struct_type;
      TYPE_FIELDS(struct_type) = field;
      tree last_field = field;

      name = get_identifier("__element_count");
      field = build_decl(BUILTINS_LOCATION, FIELD_DECL, name, sizetype);
      DECL_CONTEXT(field) = struct_type;
      DECL_CHAIN(last_field) = field;
      last_field = field;

      name = get_identifier("__bucket_count");
      field = build_decl(BUILTINS_LOCATION, FIELD_DECL, name, sizetype);
      DECL_CONTEXT(field) = struct_type;
      DECL_CHAIN(last_field) = field;
      last_field = field;

      name = get_identifier("__buckets");
      field = build_decl(BUILTINS_LOCATION, FIELD_DECL, name,
			 build_pointer_type(ptr_type_node));
      DECL_CONTEXT(field) = struct_type;
      DECL_CHAIN(last_field) = field;

      layout_type(struct_type);

      // Give the struct a name for better debugging info.
      name = get_identifier("__go_map");
      tree type_decl = build_decl(BUILTINS_LOCATION, TYPE_DECL, name,
				  struct_type);
      DECL_ARTIFICIAL(type_decl) = 1;
      TYPE_NAME(struct_type) = type_decl;
      go_preserve_from_gc(type_decl);
      rest_of_decl_compilation(type_decl, 1, 0);

      type_tree = build_pointer_type(struct_type);
      go_preserve_from_gc(type_tree);
    }

  return type_tree;
}

// Initialize a map.

tree
Map_type::do_get_init_tree(Gogo*, tree type_tree, bool is_clear)
{
  if (is_clear)
    return NULL;
  return fold_convert(type_tree, null_pointer_node);
}

// Return an expression for a newly allocated map.

tree
Map_type::do_make_expression_tree(Translate_context* context,
				  Expression_list* args,
				  source_location location)
{
  tree bad_index = NULL_TREE;

  tree expr_tree;
  if (args == NULL || args->empty())
    expr_tree = size_zero_node;
  else
    {
      expr_tree = args->front()->get_tree(context);
      if (expr_tree == error_mark_node)
	return error_mark_node;
      if (!DECL_P(expr_tree))
	expr_tree = save_expr(expr_tree);
      if (!INTEGRAL_TYPE_P(TREE_TYPE(expr_tree)))
	expr_tree = convert_to_integer(sizetype, expr_tree);
      bad_index = Expression::check_bounds(expr_tree, sizetype, bad_index,
					   location);
    }

  tree map_type = this->get_tree(context->gogo());

  static tree new_map_fndecl;
  tree ret = Gogo::call_builtin(&new_map_fndecl,
				location,
				"__go_new_map",
				2,
				map_type,
				TREE_TYPE(TYPE_FIELDS(TREE_TYPE(map_type))),
				context->gogo()->map_descriptor(this),
				sizetype,
				expr_tree);
  if (ret == error_mark_node)
    return error_mark_node;
  // This can panic if the capacity is out of range.
  TREE_NOTHROW(new_map_fndecl) = 0;

  if (bad_index == NULL_TREE)
    return ret;
  else
    {
      tree crash = Gogo::runtime_error(RUNTIME_ERROR_MAKE_MAP_OUT_OF_BOUNDS,
				       location);
      return build2(COMPOUND_EXPR, TREE_TYPE(ret),
		    build3(COND_EXPR, void_type_node,
			   bad_index, crash, NULL_TREE),
		    ret);
    }
}

// The type of a map type descriptor.

Type*
Map_type::make_map_type_descriptor_type()
{
  static Type* ret;
  if (ret == NULL)
    {
      Type* tdt = Type::make_type_descriptor_type();
      Type* ptdt = Type::make_type_descriptor_ptr_type();

      Struct_type* sf =
	Type::make_builtin_struct_type(3,
				       "", tdt,
				       "key", ptdt,
				       "elem", ptdt);

      ret = Type::make_builtin_named_type("MapType", sf);
    }

  return ret;
}

// Build a type descriptor for a map type.

Expression*
Map_type::do_type_descriptor(Gogo* gogo, Named_type* name)
{
  source_location bloc = BUILTINS_LOCATION;

  Type* mtdt = Map_type::make_map_type_descriptor_type();

  const Struct_field_list* fields = mtdt->struct_type()->fields();

  Expression_list* vals = new Expression_list();
  vals->reserve(3);

  Struct_field_list::const_iterator p = fields->begin();
  gcc_assert(p->field_name() == "commonType");
  vals->push_back(this->type_descriptor_constructor(gogo,
						    RUNTIME_TYPE_KIND_MAP,
						    name, NULL, true));

  ++p;
  gcc_assert(p->field_name() == "key");
  vals->push_back(Expression::make_type_descriptor(this->key_type_, bloc));

  ++p;
  gcc_assert(p->field_name() == "elem");
  vals->push_back(Expression::make_type_descriptor(this->val_type_, bloc));

  ++p;
  gcc_assert(p == fields->end());

  return Expression::make_struct_composite_literal(mtdt, vals, bloc);
}

// Reflection string for a map.

void
Map_type::do_reflection(Gogo* gogo, std::string* ret) const
{
  ret->append("map[");
  this->append_reflection(this->key_type_, gogo, ret);
  ret->append("] ");
  this->append_reflection(this->val_type_, gogo, ret);
}

// Mangled name for a map.

void
Map_type::do_mangled_name(Gogo* gogo, std::string* ret) const
{
  ret->push_back('M');
  this->append_mangled_name(this->key_type_, gogo, ret);
  ret->append("__");
  this->append_mangled_name(this->val_type_, gogo, ret);
}

// Export a map type.

void
Map_type::do_export(Export* exp) const
{
  exp->write_c_string("map [");
  exp->write_type(this->key_type_);
  exp->write_c_string("] ");
  exp->write_type(this->val_type_);
}

// Import a map type.

Map_type*
Map_type::do_import(Import* imp)
{
  imp->require_c_string("map [");
  Type* key_type = imp->read_type();
  imp->require_c_string("] ");
  Type* val_type = imp->read_type();
  return Type::make_map_type(key_type, val_type, imp->location());
}

// Make a map type.

Map_type*
Type::make_map_type(Type* key_type, Type* val_type, source_location location)
{
  return new Map_type(key_type, val_type, location);
}

// Class Channel_type.

// Hash code.

unsigned int
Channel_type::do_hash_for_method(Gogo* gogo) const
{
  unsigned int ret = 0;
  if (this->may_send_)
    ret += 1;
  if (this->may_receive_)
    ret += 2;
  if (this->element_type_ != NULL)
    ret += this->element_type_->hash_for_method(gogo) << 2;
  return ret << 3;
}

// Whether this type is the same as T.

bool
Channel_type::is_identical(const Channel_type* t,
			   bool errors_are_identical) const
{
  if (!Type::are_identical(this->element_type(), t->element_type(),
			   errors_are_identical, NULL))
    return false;
  return (this->may_send_ == t->may_send_
	  && this->may_receive_ == t->may_receive_);
}

// Check whether the parameters for a call to the builtin function
// make are OK for a channel.  A channel can take an optional single
// parameter which is the buffer size.

bool
Channel_type::do_check_make_expression(Expression_list* args,
				      source_location location)
{
  if (args != NULL && !args->empty())
    {
      if (!Type::check_int_value(args->front(),
				 _("bad buffer size when making channel"),
				 location))
	return false;
      else if (args->size() > 1)
	{
	  error_at(location, "too many arguments when making channel");
	  return false;
	}
    }
  return true;
}

// Return the tree for a channel type.  A channel is a pointer to a
// __go_channel struct.  The __go_channel struct is defined in
// libgo/runtime/channel.h.

tree
Channel_type::do_get_tree(Gogo*)
{
  static tree type_tree;
  if (type_tree == NULL_TREE)
    {
      tree ret = make_node(RECORD_TYPE);
      TYPE_NAME(ret) = get_identifier("__go_channel");
      TYPE_STUB_DECL(ret) = build_decl(BUILTINS_LOCATION, TYPE_DECL, NULL_TREE,
				       ret);
      type_tree = build_pointer_type(ret);
      go_preserve_from_gc(type_tree);
    }
  return type_tree;
}

// Initialize a channel variable.

tree
Channel_type::do_get_init_tree(Gogo*, tree type_tree, bool is_clear)
{
  if (is_clear)
    return NULL;
  return fold_convert(type_tree, null_pointer_node);
}

// Handle the builtin function make for a channel.

tree
Channel_type::do_make_expression_tree(Translate_context* context,
				      Expression_list* args,
				      source_location location)
{
  Gogo* gogo = context->gogo();
  tree channel_type = this->get_tree(gogo);

  tree element_tree = this->element_type_->get_tree(gogo);
  tree element_size_tree = size_in_bytes(element_tree);

  tree bad_index = NULL_TREE;

  tree expr_tree;
  if (args == NULL || args->empty())
    expr_tree = size_zero_node;
  else
    {
      expr_tree = args->front()->get_tree(context);
      if (expr_tree == error_mark_node)
	return error_mark_node;
      if (!DECL_P(expr_tree))
	expr_tree = save_expr(expr_tree);
      if (!INTEGRAL_TYPE_P(TREE_TYPE(expr_tree)))
	expr_tree = convert_to_integer(sizetype, expr_tree);
      bad_index = Expression::check_bounds(expr_tree, sizetype, bad_index,
					   location);
    }

  static tree new_channel_fndecl;
  tree ret = Gogo::call_builtin(&new_channel_fndecl,
				location,
				"__go_new_channel",
				2,
				channel_type,
				sizetype,
				element_size_tree,
				sizetype,
				expr_tree);
  if (ret == error_mark_node)
    return error_mark_node;
  // This can panic if the capacity is out of range.
  TREE_NOTHROW(new_channel_fndecl) = 0;

  if (bad_index == NULL_TREE)
    return ret;
  else
    {
      tree crash = Gogo::runtime_error(RUNTIME_ERROR_MAKE_CHAN_OUT_OF_BOUNDS,
				       location);
      return build2(COMPOUND_EXPR, TREE_TYPE(ret),
		    build3(COND_EXPR, void_type_node,
			   bad_index, crash, NULL_TREE),
		    ret);
    }
}

// Build a type descriptor for a channel type.

Type*
Channel_type::make_chan_type_descriptor_type()
{
  static Type* ret;
  if (ret == NULL)
    {
      Type* tdt = Type::make_type_descriptor_type();
      Type* ptdt = Type::make_type_descriptor_ptr_type();

      Type* uintptr_type = Type::lookup_integer_type("uintptr");

      Struct_type* sf =
	Type::make_builtin_struct_type(3,
				       "", tdt,
				       "elem", ptdt,
				       "dir", uintptr_type);

      ret = Type::make_builtin_named_type("ChanType", sf);
    }

  return ret;
}

// Build a type descriptor for a map type.

Expression*
Channel_type::do_type_descriptor(Gogo* gogo, Named_type* name)
{
  source_location bloc = BUILTINS_LOCATION;

  Type* ctdt = Channel_type::make_chan_type_descriptor_type();

  const Struct_field_list* fields = ctdt->struct_type()->fields();

  Expression_list* vals = new Expression_list();
  vals->reserve(3);

  Struct_field_list::const_iterator p = fields->begin();
  gcc_assert(p->field_name() == "commonType");
  vals->push_back(this->type_descriptor_constructor(gogo,
						    RUNTIME_TYPE_KIND_CHAN,
						    name, NULL, true));

  ++p;
  gcc_assert(p->field_name() == "elem");
  vals->push_back(Expression::make_type_descriptor(this->element_type_, bloc));

  ++p;
  gcc_assert(p->field_name() == "dir");
  // These bits must match the ones in libgo/runtime/go-type.h.
  int val = 0;
  if (this->may_receive_)
    val |= 1;
  if (this->may_send_)
    val |= 2;
  mpz_t iv;
  mpz_init_set_ui(iv, val);
  vals->push_back(Expression::make_integer(&iv, p->type(), bloc));
  mpz_clear(iv);

  ++p;
  gcc_assert(p == fields->end());

  return Expression::make_struct_composite_literal(ctdt, vals, bloc);
}

// Reflection string.

void
Channel_type::do_reflection(Gogo* gogo, std::string* ret) const
{
  if (!this->may_send_)
    ret->append("<-");
  ret->append("chan");
  if (!this->may_receive_)
    ret->append("<-");
  ret->push_back(' ');
  this->append_reflection(this->element_type_, gogo, ret);
}

// Mangled name.

void
Channel_type::do_mangled_name(Gogo* gogo, std::string* ret) const
{
  ret->push_back('C');
  this->append_mangled_name(this->element_type_, gogo, ret);
  if (this->may_send_)
    ret->push_back('s');
  if (this->may_receive_)
    ret->push_back('r');
  ret->push_back('e');
}

// Export.

void
Channel_type::do_export(Export* exp) const
{
  exp->write_c_string("chan ");
  if (this->may_send_ && !this->may_receive_)
    exp->write_c_string("-< ");
  else if (this->may_receive_ && !this->may_send_)
    exp->write_c_string("<- ");
  exp->write_type(this->element_type_);
}

// Import.

Channel_type*
Channel_type::do_import(Import* imp)
{
  imp->require_c_string("chan ");

  bool may_send;
  bool may_receive;
  if (imp->match_c_string("-< "))
    {
      imp->advance(3);
      may_send = true;
      may_receive = false;
    }
  else if (imp->match_c_string("<- "))
    {
      imp->advance(3);
      may_receive = true;
      may_send = false;
    }
  else
    {
      may_send = true;
      may_receive = true;
    }

  Type* element_type = imp->read_type();

  return Type::make_channel_type(may_send, may_receive, element_type);
}

// Make a new channel type.

Channel_type*
Type::make_channel_type(bool send, bool receive, Type* element_type)
{
  return new Channel_type(send, receive, element_type);
}

// Class Interface_type.

// Traversal.

int
Interface_type::do_traverse(Traverse* traverse)
{
  if (this->methods_ == NULL)
    return TRAVERSE_CONTINUE;
  return this->methods_->traverse(traverse);
}

// Finalize the methods.  This handles interface inheritance.

void
Interface_type::finalize_methods()
{
  if (this->methods_ == NULL)
    return;
  std::vector<Named_type*> seen;
  bool is_recursive = false;
  size_t from = 0;
  size_t to = 0;
  while (from < this->methods_->size())
    {
      const Typed_identifier* p = &this->methods_->at(from);
      if (!p->name().empty())
	{
	  size_t i;
	  for (i = 0; i < to; ++i)
	    {
	      if (this->methods_->at(i).name() == p->name())
		{
		  error_at(p->location(), "duplicate method %qs",
			   Gogo::message_name(p->name()).c_str());
		  break;
		}
	    }
	  if (i == to)
	    {
	      if (from != to)
		this->methods_->set(to, *p);
	      ++to;
	    }
	  ++from;
	  continue;
	}

      Interface_type* it = p->type()->interface_type();
      if (it == NULL)
	{
	  error_at(p->location(), "interface contains embedded non-interface");
	  ++from;
	  continue;
	}
      if (it == this)
	{
	  if (!is_recursive)
	    {
	      error_at(p->location(), "invalid recursive interface");
	      is_recursive = true;
	    }
	  ++from;
	  continue;
	}

      Named_type* nt = p->type()->named_type();
      if (nt != NULL)
	{
	  std::vector<Named_type*>::const_iterator q;
	  for (q = seen.begin(); q != seen.end(); ++q)
	    {
	      if (*q == nt)
		{
		  error_at(p->location(), "inherited interface loop");
		  break;
		}
	    }
	  if (q != seen.end())
	    {
	      ++from;
	      continue;
	    }
	  seen.push_back(nt);
	}

      const Typed_identifier_list* methods = it->methods();
      if (methods == NULL)
	{
	  ++from;
	  continue;
	}
      for (Typed_identifier_list::const_iterator q = methods->begin();
	   q != methods->end();
	   ++q)
	{
	  if (q->name().empty())
	    {
	      if (q->type()->forwarded() == p->type()->forwarded())
		error_at(p->location(), "interface inheritance loop");
	      else
		{
		  size_t i;
		  for (i = from + 1; i < this->methods_->size(); ++i)
		    {
		      const Typed_identifier* r = &this->methods_->at(i);
		      if (r->name().empty()
			  && r->type()->forwarded() == q->type()->forwarded())
			{
			  error_at(p->location(),
				   "inherited interface listed twice");
			  break;
			}
		    }
		  if (i == this->methods_->size())
		    this->methods_->push_back(Typed_identifier(q->name(),
							       q->type(),
							       p->location()));
		}
	    }
	  else if (this->find_method(q->name()) == NULL)
	    this->methods_->push_back(Typed_identifier(q->name(), q->type(),
						       p->location()));
	  else
	    {
	      if (!is_recursive)
		error_at(p->location(), "inherited method %qs is ambiguous",
			 Gogo::message_name(q->name()).c_str());
	    }
	}
      ++from;
    }
  if (to == 0)
    {
      delete this->methods_;
      this->methods_ = NULL;
    }
  else
    {
      this->methods_->resize(to);
      this->methods_->sort_by_name();
    }
}

// Return the method NAME, or NULL.

const Typed_identifier*
Interface_type::find_method(const std::string& name) const
{
  if (this->methods_ == NULL)
    return NULL;
  for (Typed_identifier_list::const_iterator p = this->methods_->begin();
       p != this->methods_->end();
       ++p)
    if (p->name() == name)
      return &*p;
  return NULL;
}

// Return the method index.

size_t
Interface_type::method_index(const std::string& name) const
{
  gcc_assert(this->methods_ != NULL);
  size_t ret = 0;
  for (Typed_identifier_list::const_iterator p = this->methods_->begin();
       p != this->methods_->end();
       ++p, ++ret)
    if (p->name() == name)
      return ret;
  gcc_unreachable();
}

// Return whether NAME is an unexported method, for better error
// reporting.

bool
Interface_type::is_unexported_method(Gogo* gogo, const std::string& name) const
{
  if (this->methods_ == NULL)
    return false;
  for (Typed_identifier_list::const_iterator p = this->methods_->begin();
       p != this->methods_->end();
       ++p)
    {
      const std::string& method_name(p->name());
      if (Gogo::is_hidden_name(method_name)
	  && name == Gogo::unpack_hidden_name(method_name)
	  && gogo->pack_hidden_name(name, false) != method_name)
	return true;
    }
  return false;
}

// Whether this type is identical with T.

bool
Interface_type::is_identical(const Interface_type* t,
			     bool errors_are_identical) const
{
  // We require the same methods with the same types.  The methods
  // have already been sorted.
  if (this->methods() == NULL || t->methods() == NULL)
    return this->methods() == t->methods();

  Typed_identifier_list::const_iterator p1 = this->methods()->begin();
  for (Typed_identifier_list::const_iterator p2 = t->methods()->begin();
       p2 != t->methods()->end();
       ++p1, ++p2)
    {
      if (p1 == this->methods()->end())
	return false;
      if (p1->name() != p2->name()
	  || !Type::are_identical(p1->type(), p2->type(),
				  errors_are_identical, NULL))
	return false;
    }
  if (p1 != this->methods()->end())
    return false;
  return true;
}

// Whether we can assign the interface type T to this type.  The types
// are known to not be identical.  An interface assignment is only
// permitted if T is known to implement all methods in THIS.
// Otherwise a type guard is required.

bool
Interface_type::is_compatible_for_assign(const Interface_type* t,
					 std::string* reason) const
{
  if (this->methods() == NULL)
    return true;
  for (Typed_identifier_list::const_iterator p = this->methods()->begin();
       p != this->methods()->end();
       ++p)
    {
      const Typed_identifier* m = t->find_method(p->name());
      if (m == NULL)
	{
	  if (reason != NULL)
	    {
	      char buf[200];
	      snprintf(buf, sizeof buf,
		       _("need explicit conversion; missing method %s%s%s"),
		       open_quote, Gogo::message_name(p->name()).c_str(),
		       close_quote);
	      reason->assign(buf);
	    }
	  return false;
	}

      std::string subreason;
      if (!Type::are_identical(p->type(), m->type(), true, &subreason))
	{
	  if (reason != NULL)
	    {
	      std::string n = Gogo::message_name(p->name());
	      size_t len = 100 + n.length() + subreason.length();
	      char* buf = new char[len];
	      if (subreason.empty())
		snprintf(buf, len, _("incompatible type for method %s%s%s"),
			 open_quote, n.c_str(), close_quote);
	      else
		snprintf(buf, len,
			 _("incompatible type for method %s%s%s (%s)"),
			 open_quote, n.c_str(), close_quote,
			 subreason.c_str());
	      reason->assign(buf);
	      delete[] buf;
	    }
	  return false;
	}
    }

  return true;
}

// Hash code.

unsigned int
Interface_type::do_hash_for_method(Gogo* gogo) const
{
  unsigned int ret = 0;
  if (this->methods_ != NULL)
    {
      for (Typed_identifier_list::const_iterator p = this->methods_->begin();
	   p != this->methods_->end();
	   ++p)
	{
	  ret = Type::hash_string(p->name(), ret);
	  ret += p->type()->hash_for_method(gogo);
	  ret <<= 1;
	}
    }
  return ret;
}

// Return true if T implements the interface.  If it does not, and
// REASON is not NULL, set *REASON to a useful error message.

bool
Interface_type::implements_interface(const Type* t, std::string* reason) const
{
  if (this->methods_ == NULL)
    return true;

  bool is_pointer = false;
  const Named_type* nt = t->named_type();
  const Struct_type* st = t->struct_type();
  // If we start with a named type, we don't dereference it to find
  // methods.
  if (nt == NULL)
    {
      const Type* pt = t->points_to();
      if (pt != NULL)
	{
	  // If T is a pointer to a named type, then we need to look at
	  // the type to which it points.
	  is_pointer = true;
	  nt = pt->named_type();
	  st = pt->struct_type();
	}
    }

  // If we have a named type, get the methods from it rather than from
  // any struct type.
  if (nt != NULL)
    st = NULL;

  // Only named and struct types have methods.
  if (nt == NULL && st == NULL)
    {
      if (reason != NULL)
	{
	  if (t->points_to() != NULL
	      && t->points_to()->interface_type() != NULL)
	    reason->assign(_("pointer to interface type has no methods"));
	  else
	    reason->assign(_("type has no methods"));
	}
      return false;
    }

  if (nt != NULL ? !nt->has_any_methods() : !st->has_any_methods())
    {
      if (reason != NULL)
	{
	  if (t->points_to() != NULL
	      && t->points_to()->interface_type() != NULL)
	    reason->assign(_("pointer to interface type has no methods"));
	  else
	    reason->assign(_("type has no methods"));
	}
      return false;
    }

  for (Typed_identifier_list::const_iterator p = this->methods_->begin();
       p != this->methods_->end();
       ++p)
    {
      bool is_ambiguous = false;
      Method* m = (nt != NULL
		   ? nt->method_function(p->name(), &is_ambiguous)
		   : st->method_function(p->name(), &is_ambiguous));
      if (m == NULL)
	{
	  if (reason != NULL)
	    {
	      std::string n = Gogo::message_name(p->name());
	      size_t len = n.length() + 100;
	      char* buf = new char[len];
	      if (is_ambiguous)
		snprintf(buf, len, _("ambiguous method %s%s%s"),
			 open_quote, n.c_str(), close_quote);
	      else
		snprintf(buf, len, _("missing method %s%s%s"),
			 open_quote, n.c_str(), close_quote);
	      reason->assign(buf);
	      delete[] buf;
	    }
	  return false;
	}

      Function_type *p_fn_type = p->type()->function_type();
      Function_type* m_fn_type = m->type()->function_type();
      gcc_assert(p_fn_type != NULL && m_fn_type != NULL);
      std::string subreason;
      if (!p_fn_type->is_identical(m_fn_type, true, true, &subreason))
	{
	  if (reason != NULL)
	    {
	      std::string n = Gogo::message_name(p->name());
	      size_t len = 100 + n.length() + subreason.length();
	      char* buf = new char[len];
	      if (subreason.empty())
		snprintf(buf, len, _("incompatible type for method %s%s%s"),
			 open_quote, n.c_str(), close_quote);
	      else
		snprintf(buf, len,
			 _("incompatible type for method %s%s%s (%s)"),
			 open_quote, n.c_str(), close_quote,
			 subreason.c_str());
	      reason->assign(buf);
	      delete[] buf;
	    }
	  return false;
	}

      if (!is_pointer && !m->is_value_method())
	{
	  if (reason != NULL)
	    {
	      std::string n = Gogo::message_name(p->name());
	      size_t len = 100 + n.length();
	      char* buf = new char[len];
	      snprintf(buf, len, _("method %s%s%s requires a pointer"),
		       open_quote, n.c_str(), close_quote);
	      reason->assign(buf);
	      delete[] buf;
	    }
	  return false;
	}
    }

  return true;
}

// Return a tree for an interface type.  An interface is a pointer to
// a struct.  The struct has three fields.  The first field is a
// pointer to the type descriptor for the dynamic type of the object.
// The second field is a pointer to a table of methods for the
// interface to be used with the object.  The third field is the value
// of the object itself.

tree
Interface_type::do_get_tree(Gogo* gogo)
{
  if (this->methods_ == NULL)
    {
      // At the tree level, use the same type for all empty
      // interfaces.  This lets us assign them to each other directly
      // without triggering GIMPLE type errors.
      tree dtype = Type::make_type_descriptor_type()->get_tree(gogo);
      dtype = build_pointer_type(build_qualified_type(dtype, TYPE_QUAL_CONST));
      static tree empty_interface;
      return Gogo::builtin_struct(&empty_interface, "__go_empty_interface",
				  NULL_TREE, 2,
				  "__type_descriptor",
				  dtype,
				  "__object",
				  ptr_type_node);
    }

  return this->fill_in_tree(gogo, make_node(RECORD_TYPE));
}

// Fill in the tree for an interface type.  This is used for named
// interface types.

tree
Interface_type::fill_in_tree(Gogo* gogo, tree type)
{
  gcc_assert(this->methods_ != NULL);

  // Because the methods may refer to the interface type itself, we
  // need to build the interface type first, and then update the
  // method pointer later.

  tree field_trees = NULL_TREE;
  tree* pp = &field_trees;

  tree name_tree = get_identifier("__methods");
  tree methods_field = build_decl(this->location_, FIELD_DECL, name_tree,
				  ptr_type_node);
  DECL_CONTEXT(methods_field) = type;
  *pp = methods_field;
  pp = &DECL_CHAIN(methods_field);

  name_tree = get_identifier("__object");
  tree field = build_decl(this->location_, FIELD_DECL, name_tree,
			  ptr_type_node);
  DECL_CONTEXT(field) = type;
  *pp = field;

  TYPE_FIELDS(type) = field_trees;

  layout_type(type);

  // Build the type of the table of methods.

  tree method_table = make_node(RECORD_TYPE);

  // The first field is a pointer to the type descriptor.
  name_tree = get_identifier("__type_descriptor");
  tree dtype = Type::make_type_descriptor_type()->get_tree(gogo);
  dtype = build_pointer_type(build_qualified_type(dtype, TYPE_QUAL_CONST));
  field = build_decl(this->location_, FIELD_DECL, name_tree, dtype);
  DECL_CONTEXT(field) = method_table;
  TYPE_FIELDS(method_table) = field;

  std::string last_name = "";
  pp = &DECL_CHAIN(field);
  for (Typed_identifier_list::const_iterator p = this->methods_->begin();
       p != this->methods_->end();
       ++p)
    {
      std::string name = Gogo::unpack_hidden_name(p->name());
      name_tree = get_identifier_with_length(name.data(), name.length());
      tree field_type = p->type()->get_tree(gogo);
      if (field_type == error_mark_node)
	return error_mark_node;
      field = build_decl(this->location_, FIELD_DECL, name_tree, field_type);
      DECL_CONTEXT(field) = method_table;
      *pp = field;
      pp = &DECL_CHAIN(field);
      // Sanity check: the names should be sorted.
      gcc_assert(p->name() > last_name);
      last_name = p->name();
    }
  layout_type(method_table);

  // Update the type of the __methods field from a generic pointer to
  // a pointer to the method table.
  TREE_TYPE(methods_field) = build_pointer_type(method_table);

  return type;
}

// Initialization value.

tree
Interface_type::do_get_init_tree(Gogo*, tree type_tree, bool is_clear)
{
  if (is_clear)
    return NULL;

  VEC(constructor_elt,gc)* init = VEC_alloc(constructor_elt, gc, 2);
  for (tree field = TYPE_FIELDS(type_tree);
       field != NULL_TREE;
       field = DECL_CHAIN(field))
    {
      constructor_elt* elt = VEC_quick_push(constructor_elt, init, NULL);
      elt->index = field;
      elt->value = fold_convert(TREE_TYPE(field), null_pointer_node);
    }

  tree ret = build_constructor(type_tree, init);
  TREE_CONSTANT(ret) = 1;
  return ret;
}

// The type of an interface type descriptor.

Type*
Interface_type::make_interface_type_descriptor_type()
{
  static Type* ret;
  if (ret == NULL)
    {
      Type* tdt = Type::make_type_descriptor_type();
      Type* ptdt = Type::make_type_descriptor_ptr_type();

      Type* string_type = Type::lookup_string_type();
      Type* pointer_string_type = Type::make_pointer_type(string_type);

      Struct_type* sm =
	Type::make_builtin_struct_type(3,
				       "name", pointer_string_type,
				       "pkgPath", pointer_string_type,
				       "typ", ptdt);

      Type* nsm = Type::make_builtin_named_type("imethod", sm);

      Type* slice_nsm = Type::make_array_type(nsm, NULL);

      Struct_type* s = Type::make_builtin_struct_type(2,
						      "", tdt,
						      "methods", slice_nsm);

      ret = Type::make_builtin_named_type("InterfaceType", s);
    }

  return ret;
}

// Build a type descriptor for an interface type.

Expression*
Interface_type::do_type_descriptor(Gogo* gogo, Named_type* name)
{
  source_location bloc = BUILTINS_LOCATION;

  Type* itdt = Interface_type::make_interface_type_descriptor_type();

  const Struct_field_list* ifields = itdt->struct_type()->fields();

  Expression_list* ivals = new Expression_list();
  ivals->reserve(2);

  Struct_field_list::const_iterator pif = ifields->begin();
  gcc_assert(pif->field_name() == "commonType");
  ivals->push_back(this->type_descriptor_constructor(gogo,
						     RUNTIME_TYPE_KIND_INTERFACE,
						     name, NULL, true));

  ++pif;
  gcc_assert(pif->field_name() == "methods");

  Expression_list* methods = new Expression_list();
  if (this->methods_ != NULL && !this->methods_->empty())
    {
      Type* elemtype = pif->type()->array_type()->element_type();

      methods->reserve(this->methods_->size());
      for (Typed_identifier_list::const_iterator pm = this->methods_->begin();
	   pm != this->methods_->end();
	   ++pm)
	{
	  const Struct_field_list* mfields = elemtype->struct_type()->fields();

	  Expression_list* mvals = new Expression_list();
	  mvals->reserve(3);

	  Struct_field_list::const_iterator pmf = mfields->begin();
	  gcc_assert(pmf->field_name() == "name");
	  std::string s = Gogo::unpack_hidden_name(pm->name());
	  Expression* e = Expression::make_string(s, bloc);
	  mvals->push_back(Expression::make_unary(OPERATOR_AND, e, bloc));

	  ++pmf;
	  gcc_assert(pmf->field_name() == "pkgPath");
	  if (!Gogo::is_hidden_name(pm->name()))
	    mvals->push_back(Expression::make_nil(bloc));
	  else
	    {
	      s = Gogo::hidden_name_prefix(pm->name());
	      e = Expression::make_string(s, bloc);
	      mvals->push_back(Expression::make_unary(OPERATOR_AND, e, bloc));
	    }

	  ++pmf;
	  gcc_assert(pmf->field_name() == "typ");
	  mvals->push_back(Expression::make_type_descriptor(pm->type(), bloc));

	  ++pmf;
	  gcc_assert(pmf == mfields->end());

	  e = Expression::make_struct_composite_literal(elemtype, mvals,
							bloc);
	  methods->push_back(e);
	}
    }

  ivals->push_back(Expression::make_slice_composite_literal(pif->type(),
							    methods, bloc));

  ++pif;
  gcc_assert(pif == ifields->end());

  return Expression::make_struct_composite_literal(itdt, ivals, bloc);
}

// Reflection string.

void
Interface_type::do_reflection(Gogo* gogo, std::string* ret) const
{
  ret->append("interface {");
  if (this->methods_ != NULL)
    {
      for (Typed_identifier_list::const_iterator p = this->methods_->begin();
	   p != this->methods_->end();
	   ++p)
	{
	  if (p != this->methods_->begin())
	    ret->append(";");
	  ret->push_back(' ');
	  ret->append(Gogo::unpack_hidden_name(p->name()));
	  std::string sub = p->type()->reflection(gogo);
	  gcc_assert(sub.compare(0, 4, "func") == 0);
	  sub = sub.substr(4);
	  ret->append(sub);
	}
    }
  ret->append(" }");
}

// Mangled name.

void
Interface_type::do_mangled_name(Gogo* gogo, std::string* ret) const
{
  ret->push_back('I');

  const Typed_identifier_list* methods = this->methods_;
  if (methods != NULL)
    {
      for (Typed_identifier_list::const_iterator p = methods->begin();
	   p != methods->end();
	   ++p)
	{
	  std::string n = Gogo::unpack_hidden_name(p->name());
	  char buf[20];
	  snprintf(buf, sizeof buf, "%u_",
		   static_cast<unsigned int>(n.length()));
	  ret->append(buf);
	  ret->append(n);
	  this->append_mangled_name(p->type(), gogo, ret);
	}
    }

  ret->push_back('e');
}

// Export.

void
Interface_type::do_export(Export* exp) const
{
  exp->write_c_string("interface { ");

  const Typed_identifier_list* methods = this->methods_;
  if (methods != NULL)
    {
      for (Typed_identifier_list::const_iterator pm = methods->begin();
	   pm != methods->end();
	   ++pm)
	{
	  exp->write_string(pm->name());
	  exp->write_c_string(" (");

	  const Function_type* fntype = pm->type()->function_type();

	  bool first = true;
	  const Typed_identifier_list* parameters = fntype->parameters();
	  if (parameters != NULL)
	    {
	      bool is_varargs = fntype->is_varargs();
	      for (Typed_identifier_list::const_iterator pp =
		     parameters->begin();
		   pp != parameters->end();
		   ++pp)
		{
		  if (first)
		    first = false;
		  else
		    exp->write_c_string(", ");
		  if (!is_varargs || pp + 1 != parameters->end())
		    exp->write_type(pp->type());
		  else
		    {
		      exp->write_c_string("...");
		      Type *pptype = pp->type();
		      exp->write_type(pptype->array_type()->element_type());
		    }
		}
	    }

	  exp->write_c_string(")");

	  const Typed_identifier_list* results = fntype->results();
	  if (results != NULL)
	    {
	      exp->write_c_string(" ");
	      if (results->size() == 1)
		exp->write_type(results->begin()->type());
	      else
		{
		  first = true;
		  exp->write_c_string("(");
		  for (Typed_identifier_list::const_iterator p =
			 results->begin();
		       p != results->end();
		       ++p)
		    {
		      if (first)
			first = false;
		      else
			exp->write_c_string(", ");
		      exp->write_type(p->type());
		    }
		  exp->write_c_string(")");
		}
	    }

	  exp->write_c_string("; ");
	}
    }

  exp->write_c_string("}");
}

// Import an interface type.

Interface_type*
Interface_type::do_import(Import* imp)
{
  imp->require_c_string("interface { ");

  Typed_identifier_list* methods = new Typed_identifier_list;
  while (imp->peek_char() != '}')
    {
      std::string name = imp->read_identifier();
      imp->require_c_string(" (");

      Typed_identifier_list* parameters;
      bool is_varargs = false;
      if (imp->peek_char() == ')')
	parameters = NULL;
      else
	{
	  parameters = new Typed_identifier_list;
	  while (true)
	    {
	      if (imp->match_c_string("..."))
		{
		  imp->advance(3);
		  is_varargs = true;
		}

	      Type* ptype = imp->read_type();
	      if (is_varargs)
		ptype = Type::make_array_type(ptype, NULL);
	      parameters->push_back(Typed_identifier(Import::import_marker,
						     ptype, imp->location()));
	      if (imp->peek_char() != ',')
		break;
	      gcc_assert(!is_varargs);
	      imp->require_c_string(", ");
	    }
	}
      imp->require_c_string(")");

      Typed_identifier_list* results;
      if (imp->peek_char() != ' ')
	results = NULL;
      else
	{
	  results = new Typed_identifier_list;
	  imp->advance(1);
	  if (imp->peek_char() != '(')
	    {
	      Type* rtype = imp->read_type();
	      results->push_back(Typed_identifier(Import::import_marker,
						  rtype, imp->location()));
	    }
	  else
	    {
	      imp->advance(1);
	      while (true)
		{
		  Type* rtype = imp->read_type();
		  results->push_back(Typed_identifier(Import::import_marker,
						      rtype, imp->location()));
		  if (imp->peek_char() != ',')
		    break;
		  imp->require_c_string(", ");
		}
	      imp->require_c_string(")");
	    }
	}

      Function_type* fntype = Type::make_function_type(NULL, parameters,
						       results,
						       imp->location());
      if (is_varargs)
	fntype->set_is_varargs();
      methods->push_back(Typed_identifier(name, fntype, imp->location()));

      imp->require_c_string("; ");
    }

  imp->require_c_string("}");

  if (methods->empty())
    {
      delete methods;
      methods = NULL;
    }

  return Type::make_interface_type(methods, imp->location());
}

// Make an interface type.

Interface_type*
Type::make_interface_type(Typed_identifier_list* methods,
			  source_location location)
{
  return new Interface_type(methods, location);
}

// Class Method.

// Bind a method to an object.

Expression*
Method::bind_method(Expression* expr, source_location location) const
{
  if (this->stub_ == NULL)
    {
      // When there is no stub object, the binding is determined by
      // the child class.
      return this->do_bind_method(expr, location);
    }

  Expression* func = Expression::make_func_reference(this->stub_, NULL,
						     location);
  return Expression::make_bound_method(expr, func, location);
}

// Return the named object associated with a method.  This may only be
// called after methods are finalized.

Named_object*
Method::named_object() const
{
  if (this->stub_ != NULL)
    return this->stub_;
  return this->do_named_object();
}

// Class Named_method.

// The type of the method.

Function_type*
Named_method::do_type() const
{
  if (this->named_object_->is_function())
    return this->named_object_->func_value()->type();
  else if (this->named_object_->is_function_declaration())
    return this->named_object_->func_declaration_value()->type();
  else
    gcc_unreachable();
}

// Return the location of the method receiver.

source_location
Named_method::do_receiver_location() const
{
  return this->do_type()->receiver()->location();
}

// Bind a method to an object.

Expression*
Named_method::do_bind_method(Expression* expr, source_location location) const
{
  Expression* func = Expression::make_func_reference(this->named_object_, NULL,
						     location);
  Bound_method_expression* bme = Expression::make_bound_method(expr, func,
							       location);
  // If this is not a local method, and it does not use a stub, then
  // the real method expects a different type.  We need to cast the
  // first argument.
  if (this->depth() > 0 && !this->needs_stub_method())
    {
      Function_type* ftype = this->do_type();
      gcc_assert(ftype->is_method());
      Type* frtype = ftype->receiver()->type();
      bme->set_first_argument_type(frtype);
    }
  return bme;
}

// Class Interface_method.

// Bind a method to an object.

Expression*
Interface_method::do_bind_method(Expression* expr,
				 source_location location) const
{
  return Expression::make_interface_field_reference(expr, this->name_,
						    location);
}

// Class Methods.

// Insert a new method.  Return true if it was inserted, false
// otherwise.

bool
Methods::insert(const std::string& name, Method* m)
{
  std::pair<Method_map::iterator, bool> ins =
    this->methods_.insert(std::make_pair(name, m));
  if (ins.second)
    return true;
  else
    {
      Method* old_method = ins.first->second;
      if (m->depth() < old_method->depth())
	{
	  delete old_method;
	  ins.first->second = m;
	  return true;
	}
      else
	{
	  if (m->depth() == old_method->depth())
	    old_method->set_is_ambiguous();
	  return false;
	}
    }
}

// Return the number of unambiguous methods.

size_t
Methods::count() const
{
  size_t ret = 0;
  for (Method_map::const_iterator p = this->methods_.begin();
       p != this->methods_.end();
       ++p)
    if (!p->second->is_ambiguous())
      ++ret;
  return ret;
}

// Class Named_type.

// Return the name of the type.

const std::string&
Named_type::name() const
{
  return this->named_object_->name();
}

// Return the name of the type to use in an error message.

std::string
Named_type::message_name() const
{
  return this->named_object_->message_name();
}

// Return the base type for this type.  We have to be careful about
// circular type definitions, which are invalid but may be seen here.

Type*
Named_type::named_base()
{
  if (this->seen_ > 0)
    return this;
  ++this->seen_;
  Type* ret = this->type_->base();
  --this->seen_;
  return ret;
}

const Type*
Named_type::named_base() const
{
  if (this->seen_ > 0)
    return this;
  ++this->seen_;
  const Type* ret = this->type_->base();
  --this->seen_;
  return ret;
}

// Return whether this is an error type.  We have to be careful about
// circular type definitions, which are invalid but may be seen here.

bool
Named_type::is_named_error_type() const
{
  if (this->seen_ > 0)
    return false;
  ++this->seen_;
  bool ret = this->type_->is_error_type();
  --this->seen_;
  return ret;
}

// Add a method to this type.

Named_object*
Named_type::add_method(const std::string& name, Function* function)
{
  if (this->local_methods_ == NULL)
    this->local_methods_ = new Bindings(NULL);
  return this->local_methods_->add_function(name, NULL, function);
}

// Add a method declaration to this type.

Named_object*
Named_type::add_method_declaration(const std::string& name, Package* package,
				   Function_type* type,
				   source_location location)
{
  if (this->local_methods_ == NULL)
    this->local_methods_ = new Bindings(NULL);
  return this->local_methods_->add_function_declaration(name, package, type,
							location);
}

// Add an existing method to this type.

void
Named_type::add_existing_method(Named_object* no)
{
  if (this->local_methods_ == NULL)
    this->local_methods_ = new Bindings(NULL);
  this->local_methods_->add_named_object(no);
}

// Look for a local method NAME, and returns its named object, or NULL
// if not there.

Named_object*
Named_type::find_local_method(const std::string& name) const
{
  if (this->local_methods_ == NULL)
    return NULL;
  return this->local_methods_->lookup(name);
}

// Return whether NAME is an unexported field or method, for better
// error reporting.

bool
Named_type::is_unexported_local_method(Gogo* gogo,
				       const std::string& name) const
{
  Bindings* methods = this->local_methods_;
  if (methods != NULL)
    {
      for (Bindings::const_declarations_iterator p =
	     methods->begin_declarations();
	   p != methods->end_declarations();
	   ++p)
	{
	  if (Gogo::is_hidden_name(p->first)
	      && name == Gogo::unpack_hidden_name(p->first)
	      && gogo->pack_hidden_name(name, false) != p->first)
	    return true;
	}
    }
  return false;
}

// Build the complete list of methods for this type, which means
// recursively including all methods for anonymous fields.  Create all
// stub methods.

void
Named_type::finalize_methods(Gogo* gogo)
{
  if (this->all_methods_ != NULL)
    return;

  if (this->local_methods_ != NULL
      && (this->points_to() != NULL || this->interface_type() != NULL))
    {
      const Bindings* lm = this->local_methods_;
      for (Bindings::const_declarations_iterator p = lm->begin_declarations();
	   p != lm->end_declarations();
	   ++p)
	error_at(p->second->location(),
		 "invalid pointer or interface receiver type");
      delete this->local_methods_;
      this->local_methods_ = NULL;
      return;
    }

  Type::finalize_methods(gogo, this, this->location_, &this->all_methods_);
}

// Return the method NAME, or NULL if there isn't one or if it is
// ambiguous.  Set *IS_AMBIGUOUS if the method exists but is
// ambiguous.

Method*
Named_type::method_function(const std::string& name, bool* is_ambiguous) const
{
  return Type::method_function(this->all_methods_, name, is_ambiguous);
}

// Return a pointer to the interface method table for this type for
// the interface INTERFACE.  IS_POINTER is true if this is for a
// pointer to THIS.

tree
Named_type::interface_method_table(Gogo* gogo, const Interface_type* interface,
				   bool is_pointer)
{
  gcc_assert(!interface->is_empty());

  Interface_method_tables** pimt = (is_pointer
				    ? &this->interface_method_tables_
				    : &this->pointer_interface_method_tables_);

  if (*pimt == NULL)
    *pimt = new Interface_method_tables(5);

  std::pair<const Interface_type*, tree> val(interface, NULL_TREE);
  std::pair<Interface_method_tables::iterator, bool> ins = (*pimt)->insert(val);

  if (ins.second)
    {
      // This is a new entry in the hash table.
      gcc_assert(ins.first->second == NULL_TREE);
      ins.first->second = gogo->interface_method_table_for_type(interface,
								this,
								is_pointer);
    }

  tree decl = ins.first->second;
  if (decl == error_mark_node)
    return error_mark_node;
  gcc_assert(decl != NULL_TREE && TREE_CODE(decl) == VAR_DECL);
  return build_fold_addr_expr(decl);
}

// Return whether a named type has any hidden fields.

bool
Named_type::named_type_has_hidden_fields(std::string* reason) const
{
  if (this->seen_ > 0)
    return false;
  ++this->seen_;
  bool ret = this->type_->has_hidden_fields(this, reason);
  --this->seen_;
  return ret;
}

// Look for a use of a complete type within another type.  This is
// used to check that we don't try to use a type within itself.

class Find_type_use : public Traverse
{
 public:
  Find_type_use(Type* find_type)
    : Traverse(traverse_types),
      find_type_(find_type), found_(false)
  { }

  // Whether we found the type.
  bool
  found() const
  { return this->found_; }

 protected:
  int
  type(Type*);

 private:
  // The type we are looking for.
  Type* find_type_;
  // Whether we found the type.
  bool found_;
};

// Check for FIND_TYPE in TYPE.

int
Find_type_use::type(Type* type)
{
  if (this->find_type_ == type)
    {
      this->found_ = true;
      return TRAVERSE_EXIT;
    }
  // It's OK if we see a reference to the type in any type which is
  // essentially a pointer: a pointer, a slice, a function, a map, or
  // a channel.
  if (type->points_to() != NULL
      || type->is_open_array_type()
      || type->function_type() != NULL
      || type->map_type() != NULL
      || type->channel_type() != NULL)
    return TRAVERSE_SKIP_COMPONENTS;

  // For an interface, a reference to the type in a method type should
  // be ignored, but we have to consider direct inheritance.  When
  // this is called, there may be cases of direct inheritance
  // represented as a method with no name.
  if (type->interface_type() != NULL)
    {
      const Typed_identifier_list* methods = type->interface_type()->methods();
      if (methods != NULL)
	{
	  for (Typed_identifier_list::const_iterator p = methods->begin();
	       p != methods->end();
	       ++p)
	    {
	      if (p->name().empty())
		{
		  if (Type::traverse(p->type(), this) == TRAVERSE_EXIT)
		    return TRAVERSE_EXIT;
		}
	    }
	}
      return TRAVERSE_SKIP_COMPONENTS;
    }

  return TRAVERSE_CONTINUE;
}

// Verify that a named type does not refer to itself.

bool
Named_type::do_verify()
{
  Find_type_use find(this);
  Type::traverse(this->type_, &find);
  if (find.found())
    {
      error_at(this->location_, "invalid recursive type %qs",
	       this->message_name().c_str());
      this->is_error_ = true;
      return false;
    }

  // Check whether any of the local methods overloads an existing
  // struct field or interface method.  We don't need to check the
  // list of methods against itself: that is handled by the Bindings
  // code.
  if (this->local_methods_ != NULL)
    {
      Struct_type* st = this->type_->struct_type();
      Interface_type* it = this->type_->interface_type();
      bool found_dup = false;
      if (st != NULL || it != NULL)
	{
	  for (Bindings::const_declarations_iterator p =
		 this->local_methods_->begin_declarations();
	       p != this->local_methods_->end_declarations();
	       ++p)
	    {
	      const std::string& name(p->first);
	      if (st != NULL && st->find_local_field(name, NULL) != NULL)
		{
		  error_at(p->second->location(),
			   "method %qs redeclares struct field name",
			   Gogo::message_name(name).c_str());
		  found_dup = true;
		}
	      if (it != NULL && it->find_method(name) != NULL)
		{
		  error_at(p->second->location(),
			   "method %qs redeclares interface method name",
			   Gogo::message_name(name).c_str());
		  found_dup = true;
		}
	    }
	}
      if (found_dup)
	return false;
    }

  // If this is a struct, then if any of the fields of the struct
  // themselves have struct type, or array of struct type, then this
  // struct must be converted to the backend representation before the
  // field's type is converted.  That may seem backward, but it works
  // because if the field's type refers to this one, e.g., via a
  // pointer, then the conversion process will pick up the half-built
  // struct and do the right thing.
  if (this->struct_type() != NULL)
    {
      const Struct_field_list* fields = this->struct_type()->fields();
      for (Struct_field_list::const_iterator p = fields->begin();
	   p != fields->end();
	   ++p)
	{
	  Struct_type* st = p->type()->struct_type();
	  if (st != NULL)
	    st->add_prerequisite(this);
	  else
	    {
	      Array_type* at = p->type()->array_type();
	      if (at != NULL && !at->is_open_array_type())
		{
		  st = at->element_type()->struct_type();
		  if (st != NULL)
		    st->add_prerequisite(this);
		}
	    }
	}
    }

  return true;
}

// Return whether this type is or contains a pointer.

bool
Named_type::do_has_pointer() const
{
  if (this->seen_ > 0)
    return false;
  ++this->seen_;
  bool ret = this->type_->has_pointer();
  --this->seen_;
  return ret;
}

// Return a hash code.  This is used for method lookup.  We simply
// hash on the name itself.

unsigned int
Named_type::do_hash_for_method(Gogo* gogo) const
{
  const std::string& name(this->named_object()->name());
  unsigned int ret = Type::hash_string(name, 0);

  // GOGO will be NULL here when called from Type_hash_identical.
  // That is OK because that is only used for internal hash tables
  // where we are going to be comparing named types for equality.  In
  // other cases, which are cases where the runtime is going to
  // compare hash codes to see if the types are the same, we need to
  // include the package prefix and name in the hash.
  if (gogo != NULL && !Gogo::is_hidden_name(name) && !this->is_builtin())
    {
      const Package* package = this->named_object()->package();
      if (package == NULL)
	{
	  ret = Type::hash_string(gogo->unique_prefix(), ret);
	  ret = Type::hash_string(gogo->package_name(), ret);
	}
      else
	{
	  ret = Type::hash_string(package->unique_prefix(), ret);
	  ret = Type::hash_string(package->name(), ret);
	}
    }

  return ret;
}

// Get a tree for a named type.

tree
Named_type::do_get_tree(Gogo* gogo)
{
  if (this->is_error_)
    return error_mark_node;

  // Go permits types to refer to themselves in various ways.  Break
  // the recursion here.
  tree t;
  switch (this->type_->forwarded()->classification())
    {
    case TYPE_ERROR:
      return error_mark_node;

    case TYPE_VOID:
    case TYPE_BOOLEAN:
    case TYPE_INTEGER:
    case TYPE_FLOAT:
    case TYPE_COMPLEX:
    case TYPE_STRING:
    case TYPE_NIL:
      // These types can not refer to themselves.
    case TYPE_MAP:
    case TYPE_CHANNEL:
      // All maps and channels have the same type in GENERIC.
      t = Type::get_named_type_tree(gogo, this->type_);
      if (t == error_mark_node)
	return error_mark_node;
      // Build a copy to set TYPE_NAME.
      t = build_variant_type_copy(t);
      break;

    case TYPE_FUNCTION:
      // GENERIC can't handle a pointer to a function type whose
      // return type is a pointer to the function type itself.  It
      // goes into an infinite loop when walking the types.
      if (this->seen_ > 0)
	{
	  Function_type* fntype = this->type_->function_type();
	  if (fntype->results() != NULL
	      && fntype->results()->size() == 1
	      && fntype->results()->front().type()->forwarded() == this)
	    return ptr_type_node;

	  // We can legitimately see ourselves here twice when a named
	  // type is defined using a struct which refers to the named
	  // type.  If we see ourselves too often we are in a loop.
	  if (this->seen_ > 3)
	    return ptr_type_node;
	}
      ++this->seen_;
      t = Type::get_named_type_tree(gogo, this->type_);
      --this->seen_;
      if (t == error_mark_node)
	return error_mark_node;
      t = build_variant_type_copy(t);
      break;

    case TYPE_POINTER:
      // Don't recur infinitely if a pointer type refers to itself.
      // Ideally we would build a circular data structure here, but
      // GENERIC can't handle them.
      if (this->seen_ > 0)
	{
	  if (this->type_->points_to()->forwarded() == this)
	    return ptr_type_node;

	  if (this->seen_ > 3)
	    return ptr_type_node;
	}
      ++this->seen_;
      t = Type::get_named_type_tree(gogo, this->type_);
      --this->seen_;
      if (t == error_mark_node)
	return error_mark_node;
      t = build_variant_type_copy(t);
      break;

    case TYPE_STRUCT:
      // If there are structs which must be converted first, do them.
      if (this->seen_ == 0)
	{
	  ++this->seen_;
	  this->type_->struct_type()->convert_prerequisites(gogo);
	  --this->seen_;
	}

      if (this->named_tree_ != NULL_TREE)
	return this->named_tree_;

      t = make_node(RECORD_TYPE);
      this->named_tree_ = t;
      t = this->type_->struct_type()->fill_in_tree(gogo, t);
      if (t == error_mark_node)
	{
	  this->named_tree_ = error_mark_node;
	  return error_mark_node;
	}
      break;

    case TYPE_ARRAY:
      if (this->named_tree_ != NULL_TREE)
	return this->named_tree_;
      if (!this->is_open_array_type())
	{
	  t = make_node(ARRAY_TYPE);
	  this->named_tree_ = t;
	  t = this->type_->array_type()->fill_in_array_tree(gogo, t);
	}
      else
	{
	  t = gogo->slice_type_tree(void_type_node);
	  this->named_tree_ = t;
	  t = this->type_->array_type()->fill_in_slice_tree(gogo, t);
	}
      if (t == error_mark_node)
	return error_mark_node;
      t = build_variant_type_copy(t);
      break;

    case TYPE_INTERFACE:
      if (this->type_->interface_type()->is_empty())
	{
	  t = Type::get_named_type_tree(gogo, this->type_);
	  if (t == error_mark_node)
	    return error_mark_node;
	  t = build_variant_type_copy(t);
	}
      else
	{
	  if (this->named_tree_ != NULL_TREE)
	    return this->named_tree_;
	  t = make_node(RECORD_TYPE);
	  this->named_tree_ = t;
	  t = this->type_->interface_type()->fill_in_tree(gogo, t);
	  if (t == error_mark_node)
	    {
	      this->named_tree_ = error_mark_node;
	      return error_mark_node;
	    }
	}
      break;

    case TYPE_NAMED:
      {
	// When a named type T1 is defined as another named type T2,
	// the definition must simply be "type T1 T2".  If the
	// definition of T2 may refer to T1, then we must simply
	// return the type for T2 here.  It's not precisely correct,
	// but it's as close as we can get with GENERIC.
	++this->seen_;
	t = Type::get_named_type_tree(gogo, this->type_);
	--this->seen_;
	if (this->seen_ > 0)
	  return t;
	if (t == error_mark_node)
	  return error_mark_node;
	t = build_variant_type_copy(t);
      }
      break;

    case TYPE_FORWARD:
      // An undefined forwarding type.  Make sure the error is
      // emitted.
      this->type_->forward_declaration_type()->real_type();
      return error_mark_node;

    default:
    case TYPE_SINK:
    case TYPE_CALL_MULTIPLE_RESULT:
      gcc_unreachable();
    }

  tree id = this->named_object_->get_id(gogo);
  tree decl = build_decl(this->location_, TYPE_DECL, id, t);
  TYPE_NAME(t) = decl;

  return t;
}

// Build a type descriptor for a named type.

Expression*
Named_type::do_type_descriptor(Gogo* gogo, Named_type* name)
{
  // If NAME is not NULL, then we don't really want the type
  // descriptor for this type; we want the descriptor for the
  // underlying type, giving it the name NAME.
  return this->named_type_descriptor(gogo, this->type_,
				     name == NULL ? this : name);
}

// Add to the reflection string.  This is used mostly for the name of
// the type used in a type descriptor, not for actual reflection
// strings.

void
Named_type::do_reflection(Gogo* gogo, std::string* ret) const
{
  if (this->location() != BUILTINS_LOCATION)
    {
      const Package* package = this->named_object_->package();
      if (package != NULL)
	ret->append(package->name());
      else
	ret->append(gogo->package_name());
      ret->push_back('.');
    }
  if (this->in_function_ != NULL)
    {
      ret->append(Gogo::unpack_hidden_name(this->in_function_->name()));
      ret->push_back('$');
    }
  ret->append(Gogo::unpack_hidden_name(this->named_object_->name()));
}

// Get the mangled name.

void
Named_type::do_mangled_name(Gogo* gogo, std::string* ret) const
{
  Named_object* no = this->named_object_;
  std::string name;
  if (this->location() == BUILTINS_LOCATION)
    gcc_assert(this->in_function_ == NULL);
  else
    {
      const std::string& unique_prefix(no->package() == NULL
				       ? gogo->unique_prefix()
				       : no->package()->unique_prefix());
      const std::string& package_name(no->package() == NULL
				      ? gogo->package_name()
				      : no->package()->name());
      name = unique_prefix;
      name.append(1, '.');
      name.append(package_name);
      name.append(1, '.');
      if (this->in_function_ != NULL)
	{
	  name.append(Gogo::unpack_hidden_name(this->in_function_->name()));
	  name.append(1, '$');
	}
    }
  name.append(Gogo::unpack_hidden_name(no->name()));
  char buf[20];
  snprintf(buf, sizeof buf, "N%u_", static_cast<unsigned int>(name.length()));
  ret->append(buf);
  ret->append(name);
}

// Export the type.  This is called to export a global type.

void
Named_type::export_named_type(Export* exp, const std::string&) const
{
  // We don't need to write the name of the type here, because it will
  // be written by Export::write_type anyhow.
  exp->write_c_string("type ");
  exp->write_type(this);
  exp->write_c_string(";\n");
}

// Import a named type.

void
Named_type::import_named_type(Import* imp, Named_type** ptype)
{
  imp->require_c_string("type ");
  Type *type = imp->read_type();
  *ptype = type->named_type();
  gcc_assert(*ptype != NULL);
  imp->require_c_string(";\n");
}

// Export the type when it is referenced by another type.  In this
// case Export::export_type will already have issued the name.

void
Named_type::do_export(Export* exp) const
{
  exp->write_type(this->type_);

  // To save space, we only export the methods directly attached to
  // this type.
  Bindings* methods = this->local_methods_;
  if (methods == NULL)
    return;

  exp->write_c_string("\n");
  for (Bindings::const_definitions_iterator p = methods->begin_definitions();
       p != methods->end_definitions();
       ++p)
    {
      exp->write_c_string(" ");
      (*p)->export_named_object(exp);
    }

  for (Bindings::const_declarations_iterator p = methods->begin_declarations();
       p != methods->end_declarations();
       ++p)
    {
      if (p->second->is_function_declaration())
	{
	  exp->write_c_string(" ");
	  p->second->export_named_object(exp);
	}
    }
}

// Make a named type.

Named_type*
Type::make_named_type(Named_object* named_object, Type* type,
		      source_location location)
{
  return new Named_type(named_object, type, location);
}

// Finalize the methods for TYPE.  It will be a named type or a struct
// type.  This sets *ALL_METHODS to the list of methods, and builds
// all required stubs.

void
Type::finalize_methods(Gogo* gogo, const Type* type, source_location location,
		       Methods** all_methods)
{
  *all_methods = NULL;
  Types_seen types_seen;
  Type::add_methods_for_type(type, NULL, 0, false, false, &types_seen,
			     all_methods);
  Type::build_stub_methods(gogo, type, *all_methods, location);
}

// Add the methods for TYPE to *METHODS.  FIELD_INDEXES is used to
// build up the struct field indexes as we go.  DEPTH is the depth of
// the field within TYPE.  IS_EMBEDDED_POINTER is true if we are
// adding these methods for an anonymous field with pointer type.
// NEEDS_STUB_METHOD is true if we need to use a stub method which
// calls the real method.  TYPES_SEEN is used to avoid infinite
// recursion.

void
Type::add_methods_for_type(const Type* type,
			   const Method::Field_indexes* field_indexes,
			   unsigned int depth,
			   bool is_embedded_pointer,
			   bool needs_stub_method,
			   Types_seen* types_seen,
			   Methods** methods)
{
  // Pointer types may not have methods.
  if (type->points_to() != NULL)
    return;

  const Named_type* nt = type->named_type();
  if (nt != NULL)
    {
      std::pair<Types_seen::iterator, bool> ins = types_seen->insert(nt);
      if (!ins.second)
	return;
    }

  if (nt != NULL)
    Type::add_local_methods_for_type(nt, field_indexes, depth,
				     is_embedded_pointer, needs_stub_method,
				     methods);

  Type::add_embedded_methods_for_type(type, field_indexes, depth,
				      is_embedded_pointer, needs_stub_method,
				      types_seen, methods);

  // If we are called with depth > 0, then we are looking at an
  // anonymous field of a struct.  If such a field has interface type,
  // then we need to add the interface methods.  We don't want to add
  // them when depth == 0, because we will already handle them
  // following the usual rules for an interface type.
  if (depth > 0)
    Type::add_interface_methods_for_type(type, field_indexes, depth, methods);
}

// Add the local methods for the named type NT to *METHODS.  The
// parameters are as for add_methods_to_type.

void
Type::add_local_methods_for_type(const Named_type* nt,
				 const Method::Field_indexes* field_indexes,
				 unsigned int depth,
				 bool is_embedded_pointer,
				 bool needs_stub_method,
				 Methods** methods)
{
  const Bindings* local_methods = nt->local_methods();
  if (local_methods == NULL)
    return;

  if (*methods == NULL)
    *methods = new Methods();

  for (Bindings::const_declarations_iterator p =
	 local_methods->begin_declarations();
       p != local_methods->end_declarations();
       ++p)
    {
      Named_object* no = p->second;
      bool is_value_method = (is_embedded_pointer
			      || !Type::method_expects_pointer(no));
      Method* m = new Named_method(no, field_indexes, depth, is_value_method,
				   (needs_stub_method
				    || (depth > 0 && is_value_method)));
      if (!(*methods)->insert(no->name(), m))
	delete m;
    }
}

// Add the embedded methods for TYPE to *METHODS.  These are the
// methods attached to anonymous fields.  The parameters are as for
// add_methods_to_type.

void
Type::add_embedded_methods_for_type(const Type* type,
				    const Method::Field_indexes* field_indexes,
				    unsigned int depth,
				    bool is_embedded_pointer,
				    bool needs_stub_method,
				    Types_seen* types_seen,
				    Methods** methods)
{
  // Look for anonymous fields in TYPE.  TYPE has fields if it is a
  // struct.
  const Struct_type* st = type->struct_type();
  if (st == NULL)
    return;

  const Struct_field_list* fields = st->fields();
  if (fields == NULL)
    return;

  unsigned int i = 0;
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf, ++i)
    {
      if (!pf->is_anonymous())
	continue;

      Type* ftype = pf->type();
      bool is_pointer = false;
      if (ftype->points_to() != NULL)
	{
	  ftype = ftype->points_to();
	  is_pointer = true;
	}
      Named_type* fnt = ftype->named_type();
      if (fnt == NULL)
	{
	  // This is an error, but it will be diagnosed elsewhere.
	  continue;
	}

      Method::Field_indexes* sub_field_indexes = new Method::Field_indexes();
      sub_field_indexes->next = field_indexes;
      sub_field_indexes->field_index = i;

      Type::add_methods_for_type(fnt, sub_field_indexes, depth + 1,
				 (is_embedded_pointer || is_pointer),
				 (needs_stub_method
				  || is_pointer
				  || i > 0),
				 types_seen,
				 methods);
    }
}

// If TYPE is an interface type, then add its method to *METHODS.
// This is for interface methods attached to an anonymous field.  The
// parameters are as for add_methods_for_type.

void
Type::add_interface_methods_for_type(const Type* type,
				     const Method::Field_indexes* field_indexes,
				     unsigned int depth,
				     Methods** methods)
{
  const Interface_type* it = type->interface_type();
  if (it == NULL)
    return;

  const Typed_identifier_list* imethods = it->methods();
  if (imethods == NULL)
    return;

  if (*methods == NULL)
    *methods = new Methods();

  for (Typed_identifier_list::const_iterator pm = imethods->begin();
       pm != imethods->end();
       ++pm)
    {
      Function_type* fntype = pm->type()->function_type();
      if (fntype == NULL)
	{
	  // This is an error, but it should be reported elsewhere
	  // when we look at the methods for IT.
	  continue;
	}
      gcc_assert(!fntype->is_method());
      fntype = fntype->copy_with_receiver(const_cast<Type*>(type));
      Method* m = new Interface_method(pm->name(), pm->location(), fntype,
				       field_indexes, depth);
      if (!(*methods)->insert(pm->name(), m))
	delete m;
    }
}

// Build stub methods for TYPE as needed.  METHODS is the set of
// methods for the type.  A stub method may be needed when a type
// inherits a method from an anonymous field.  When we need the
// address of the method, as in a type descriptor, we need to build a
// little stub which does the required field dereferences and jumps to
// the real method.  LOCATION is the location of the type definition.

void
Type::build_stub_methods(Gogo* gogo, const Type* type, const Methods* methods,
			 source_location location)
{
  if (methods == NULL)
    return;
  for (Methods::const_iterator p = methods->begin();
       p != methods->end();
       ++p)
    {
      Method* m = p->second;
      if (m->is_ambiguous() || !m->needs_stub_method())
	continue;

      const std::string& name(p->first);

      // Build a stub method.

      const Function_type* fntype = m->type();

      static unsigned int counter;
      char buf[100];
      snprintf(buf, sizeof buf, "$this%u", counter);
      ++counter;

      Type* receiver_type = const_cast<Type*>(type);
      if (!m->is_value_method())
	receiver_type = Type::make_pointer_type(receiver_type);
      source_location receiver_location = m->receiver_location();
      Typed_identifier* receiver = new Typed_identifier(buf, receiver_type,
							receiver_location);

      const Typed_identifier_list* fnparams = fntype->parameters();
      Typed_identifier_list* stub_params;
      if (fnparams == NULL || fnparams->empty())
	stub_params = NULL;
      else
	{
	  // We give each stub parameter a unique name.
	  stub_params = new Typed_identifier_list();
	  for (Typed_identifier_list::const_iterator pp = fnparams->begin();
	       pp != fnparams->end();
	       ++pp)
	    {
	      char pbuf[100];
	      snprintf(pbuf, sizeof pbuf, "$p%u", counter);
	      stub_params->push_back(Typed_identifier(pbuf, pp->type(),
						      pp->location()));
	      ++counter;
	    }
	}

      const Typed_identifier_list* fnresults = fntype->results();
      Typed_identifier_list* stub_results;
      if (fnresults == NULL || fnresults->empty())
	stub_results = NULL;
      else
	{
	  // We create the result parameters without any names, since
	  // we won't refer to them.
	  stub_results = new Typed_identifier_list();
	  for (Typed_identifier_list::const_iterator pr = fnresults->begin();
	       pr != fnresults->end();
	       ++pr)
	    stub_results->push_back(Typed_identifier("", pr->type(),
						     pr->location()));
	}

      Function_type* stub_type = Type::make_function_type(receiver,
							  stub_params,
							  stub_results,
							  fntype->location());
      if (fntype->is_varargs())
	stub_type->set_is_varargs();

      // We only create the function in the package which creates the
      // type.
      const Package* package;
      if (type->named_type() == NULL)
	package = NULL;
      else
	package = type->named_type()->named_object()->package();
      Named_object* stub;
      if (package != NULL)
	stub = Named_object::make_function_declaration(name, package,
						       stub_type, location);
      else
	{
	  stub = gogo->start_function(name, stub_type, false,
				      fntype->location());
	  Type::build_one_stub_method(gogo, m, buf, stub_params,
				      fntype->is_varargs(), location);
	  gogo->finish_function(fntype->location());
	}

      m->set_stub_object(stub);
    }
}

// Build a stub method which adjusts the receiver as required to call
// METHOD.  RECEIVER_NAME is the name we used for the receiver.
// PARAMS is the list of function parameters.

void
Type::build_one_stub_method(Gogo* gogo, Method* method,
			    const char* receiver_name,
			    const Typed_identifier_list* params,
			    bool is_varargs,
			    source_location location)
{
  Named_object* receiver_object = gogo->lookup(receiver_name, NULL);
  gcc_assert(receiver_object != NULL);

  Expression* expr = Expression::make_var_reference(receiver_object, location);
  expr = Type::apply_field_indexes(expr, method->field_indexes(), location);
  if (expr->type()->points_to() == NULL)
    expr = Expression::make_unary(OPERATOR_AND, expr, location);

  Expression_list* arguments;
  if (params == NULL || params->empty())
    arguments = NULL;
  else
    {
      arguments = new Expression_list();
      for (Typed_identifier_list::const_iterator p = params->begin();
	   p != params->end();
	   ++p)
	{
	  Named_object* param = gogo->lookup(p->name(), NULL);
	  gcc_assert(param != NULL);
	  Expression* param_ref = Expression::make_var_reference(param,
								 location);
	  arguments->push_back(param_ref);
	}
    }

  Expression* func = method->bind_method(expr, location);
  gcc_assert(func != NULL);
  Call_expression* call = Expression::make_call(func, arguments, is_varargs,
						location);
  size_t count = call->result_count();
  if (count == 0)
    gogo->add_statement(Statement::make_statement(call));
  else
    {
      Expression_list* retvals = new Expression_list();
      if (count <= 1)
	retvals->push_back(call);
      else
	{
	  for (size_t i = 0; i < count; ++i)
	    retvals->push_back(Expression::make_call_result(call, i));
	}
      const Function* function = gogo->current_function()->func_value();
      const Typed_identifier_list* results = function->type()->results();
      Statement* retstat = Statement::make_return_statement(results, retvals,
							    location);
      gogo->add_statement(retstat);
    }
}

// Apply FIELD_INDEXES to EXPR.  The field indexes have to be applied
// in reverse order.

Expression*
Type::apply_field_indexes(Expression* expr,
			  const Method::Field_indexes* field_indexes,
			  source_location location)
{
  if (field_indexes == NULL)
    return expr;
  expr = Type::apply_field_indexes(expr, field_indexes->next, location);
  Struct_type* stype = expr->type()->deref()->struct_type();
  gcc_assert(stype != NULL
	     && field_indexes->field_index < stype->field_count());
  if (expr->type()->struct_type() == NULL)
    {
      gcc_assert(expr->type()->points_to() != NULL);
      expr = Expression::make_unary(OPERATOR_MULT, expr, location);
      gcc_assert(expr->type()->struct_type() == stype);
    }
  return Expression::make_field_reference(expr, field_indexes->field_index,
					  location);
}

// Return whether NO is a method for which the receiver is a pointer.

bool
Type::method_expects_pointer(const Named_object* no)
{
  const Function_type *fntype;
  if (no->is_function())
    fntype = no->func_value()->type();
  else if (no->is_function_declaration())
    fntype = no->func_declaration_value()->type();
  else
    gcc_unreachable();
  return fntype->receiver()->type()->points_to() != NULL;
}

// Given a set of methods for a type, METHODS, return the method NAME,
// or NULL if there isn't one or if it is ambiguous.  If IS_AMBIGUOUS
// is not NULL, then set *IS_AMBIGUOUS to true if the method exists
// but is ambiguous (and return NULL).

Method*
Type::method_function(const Methods* methods, const std::string& name,
		      bool* is_ambiguous)
{
  if (is_ambiguous != NULL)
    *is_ambiguous = false;
  if (methods == NULL)
    return NULL;
  Methods::const_iterator p = methods->find(name);
  if (p == methods->end())
    return NULL;
  Method* m = p->second;
  if (m->is_ambiguous())
    {
      if (is_ambiguous != NULL)
	*is_ambiguous = true;
      return NULL;
    }
  return m;
}

// Look for field or method NAME for TYPE.  Return an Expression for
// the field or method bound to EXPR.  If there is no such field or
// method, give an appropriate error and return an error expression.

Expression*
Type::bind_field_or_method(Gogo* gogo, const Type* type, Expression* expr,
			   const std::string& name,
			   source_location location)
{
  if (type->deref()->is_error_type())
    return Expression::make_error(location);

  const Named_type* nt = type->named_type();
  if (nt == NULL)
    nt = type->deref()->named_type();
  const Struct_type* st = type->deref()->struct_type();
  const Interface_type* it = type->deref()->interface_type();

  // If this is a pointer to a pointer, then it is possible that the
  // pointed-to type has methods.
  if (nt == NULL
      && st == NULL
      && it == NULL
      && type->points_to() != NULL
      && type->points_to()->points_to() != NULL)
    {
      expr = Expression::make_unary(OPERATOR_MULT, expr, location);
      type = type->points_to();
      if (type->deref()->is_error_type())
	return Expression::make_error(location);
      nt = type->points_to()->named_type();
      st = type->points_to()->struct_type();
      it = type->points_to()->interface_type();
    }

  bool receiver_can_be_pointer = (expr->type()->points_to() != NULL
				  || expr->is_addressable());
  std::vector<const Named_type*> seen;
  bool is_method = false;
  bool found_pointer_method = false;
  std::string ambig1;
  std::string ambig2;
  if (Type::find_field_or_method(type, name, receiver_can_be_pointer,
				 &seen, NULL, &is_method,
				 &found_pointer_method, &ambig1, &ambig2))
    {
      Expression* ret;
      if (!is_method)
	{
	  gcc_assert(st != NULL);
	  if (type->struct_type() == NULL)
	    {
	      gcc_assert(type->points_to() != NULL);
	      expr = Expression::make_unary(OPERATOR_MULT, expr,
					    location);
	      gcc_assert(expr->type()->struct_type() == st);
	    }
	  ret = st->field_reference(expr, name, location);
	}
      else if (it != NULL && it->find_method(name) != NULL)
	ret = Expression::make_interface_field_reference(expr, name,
							 location);
      else
	{
	  Method* m;
	  if (nt != NULL)
	    m = nt->method_function(name, NULL);
	  else if (st != NULL)
	    m = st->method_function(name, NULL);
	  else
	    gcc_unreachable();
	  gcc_assert(m != NULL);
	  if (!m->is_value_method() && expr->type()->points_to() == NULL)
	    expr = Expression::make_unary(OPERATOR_AND, expr, location);
	  ret = m->bind_method(expr, location);
	}
      gcc_assert(ret != NULL);
      return ret;
    }
  else
    {
      if (!ambig1.empty())
	error_at(location, "%qs is ambiguous via %qs and %qs",
		 Gogo::message_name(name).c_str(),
		 Gogo::message_name(ambig1).c_str(),
		 Gogo::message_name(ambig2).c_str());
      else if (found_pointer_method)
	error_at(location, "method requires a pointer");
      else if (nt == NULL && st == NULL && it == NULL)
	error_at(location,
		 ("reference to field %qs in object which "
		  "has no fields or methods"),
		 Gogo::message_name(name).c_str());
      else
	{
	  bool is_unexported;
	  if (!Gogo::is_hidden_name(name))
	    is_unexported = false;
	  else
	    {
	      std::string unpacked = Gogo::unpack_hidden_name(name);
	      seen.clear();
	      is_unexported = Type::is_unexported_field_or_method(gogo, type,
								  unpacked,
								  &seen);
	    }
	  if (is_unexported)
	    error_at(location, "reference to unexported field or method %qs",
		     Gogo::message_name(name).c_str());
	  else
	    error_at(location, "reference to undefined field or method %qs",
		     Gogo::message_name(name).c_str());
	}
      return Expression::make_error(location);
    }
}

// Look in TYPE for a field or method named NAME, return true if one
// is found.  This looks through embedded anonymous fields and handles
// ambiguity.  If a method is found, sets *IS_METHOD to true;
// otherwise, if a field is found, set it to false.  If
// RECEIVER_CAN_BE_POINTER is false, then the receiver is a value
// whose address can not be taken.  SEEN is used to avoid infinite
// recursion on invalid types.

// When returning false, this sets *FOUND_POINTER_METHOD if we found a
// method we couldn't use because it requires a pointer.  LEVEL is
// used for recursive calls, and can be NULL for a non-recursive call.
// When this function returns false because it finds that the name is
// ambiguous, it will store a path to the ambiguous names in *AMBIG1
// and *AMBIG2.  If the name is not found at all, *AMBIG1 and *AMBIG2
// will be unchanged.

// This function just returns whether or not there is a field or
// method, and whether it is a field or method.  It doesn't build an
// expression to refer to it.  If it is a method, we then look in the
// list of all methods for the type.  If it is a field, the search has
// to be done again, looking only for fields, and building up the
// expression as we go.

bool
Type::find_field_or_method(const Type* type,
			   const std::string& name,
			   bool receiver_can_be_pointer,
			   std::vector<const Named_type*>* seen,
			   int* level,
			   bool* is_method,
			   bool* found_pointer_method,
			   std::string* ambig1,
			   std::string* ambig2)
{
  // Named types can have locally defined methods.
  const Named_type* nt = type->named_type();
  if (nt == NULL && type->points_to() != NULL)
    nt = type->points_to()->named_type();
  if (nt != NULL)
    {
      Named_object* no = nt->find_local_method(name);
      if (no != NULL)
	{
	  if (receiver_can_be_pointer || !Type::method_expects_pointer(no))
	    {
	      *is_method = true;
	      return true;
	    }

	  // Record that we have found a pointer method in order to
	  // give a better error message if we don't find anything
	  // else.
	  *found_pointer_method = true;
	}

      for (std::vector<const Named_type*>::const_iterator p = seen->begin();
	   p != seen->end();
	   ++p)
	{
	  if (*p == nt)
	    {
	      // We've already seen this type when searching for methods.
	      return false;
	    }
	}
    }

  // Interface types can have methods.
  const Interface_type* it = type->deref()->interface_type();
  if (it != NULL && it->find_method(name) != NULL)
    {
      *is_method = true;
      return true;
    }

  // Struct types can have fields.  They can also inherit fields and
  // methods from anonymous fields.
  const Struct_type* st = type->deref()->struct_type();
  if (st == NULL)
    return false;
  const Struct_field_list* fields = st->fields();
  if (fields == NULL)
    return false;

  if (nt != NULL)
    seen->push_back(nt);

  int found_level = 0;
  bool found_is_method = false;
  std::string found_ambig1;
  std::string found_ambig2;
  const Struct_field* found_parent = NULL;
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf)
    {
      if (pf->field_name() == name)
	{
	  *is_method = false;
	  if (nt != NULL)
	    seen->pop_back();
	  return true;
	}

      if (!pf->is_anonymous())
	continue;

      if (pf->type()->deref()->is_error_type()
	  || pf->type()->deref()->is_undefined())
	continue;

      Named_type* fnt = pf->type()->named_type();
      if (fnt == NULL)
	fnt = pf->type()->deref()->named_type();
      gcc_assert(fnt != NULL);

      int sublevel = level == NULL ? 1 : *level + 1;
      bool sub_is_method;
      std::string subambig1;
      std::string subambig2;
      bool subfound = Type::find_field_or_method(fnt,
						 name,
						 receiver_can_be_pointer,
						 seen,
						 &sublevel,
						 &sub_is_method,
						 found_pointer_method,
						 &subambig1,
						 &subambig2);
      if (!subfound)
	{
	  if (!subambig1.empty())
	    {
	      // The name was found via this field, but is ambiguous.
	      // if the ambiguity is lower or at the same level as
	      // anything else we have already found, then we want to
	      // pass the ambiguity back to the caller.
	      if (found_level == 0 || sublevel <= found_level)
		{
		  found_ambig1 = pf->field_name() + '.' + subambig1;
		  found_ambig2 = pf->field_name() + '.' + subambig2;
		  found_level = sublevel;
		}
	    }
	}
      else
	{
	  // The name was found via this field.  Use the level to see
	  // if we want to use this one, or whether it introduces an
	  // ambiguity.
	  if (found_level == 0 || sublevel < found_level)
	    {
	      found_level = sublevel;
	      found_is_method = sub_is_method;
	      found_ambig1.clear();
	      found_ambig2.clear();
	      found_parent = &*pf;
	    }
	  else if (sublevel > found_level)
	    ;
	  else if (found_ambig1.empty())
	    {
	      // We found an ambiguity.
	      gcc_assert(found_parent != NULL);
	      found_ambig1 = found_parent->field_name();
	      found_ambig2 = pf->field_name();
	    }
	  else
	    {
	      // We found an ambiguity, but we already know of one.
	      // Just report the earlier one.
	    }
	}
    }

  // Here if we didn't find anything FOUND_LEVEL is 0.  If we found
  // something ambiguous, FOUND_LEVEL is not 0 and FOUND_AMBIG1 and
  // FOUND_AMBIG2 are not empty.  If we found the field, FOUND_LEVEL
  // is not 0 and FOUND_AMBIG1 and FOUND_AMBIG2 are empty.

  if (nt != NULL)
    seen->pop_back();

  if (found_level == 0)
    return false;
  else if (!found_ambig1.empty())
    {
      gcc_assert(!found_ambig1.empty());
      ambig1->assign(found_ambig1);
      ambig2->assign(found_ambig2);
      if (level != NULL)
	*level = found_level;
      return false;
    }
  else
    {
      if (level != NULL)
	*level = found_level;
      *is_method = found_is_method;
      return true;
    }
}

// Return whether NAME is an unexported field or method for TYPE.

bool
Type::is_unexported_field_or_method(Gogo* gogo, const Type* type,
				    const std::string& name,
				    std::vector<const Named_type*>* seen)
{
  const Named_type* nt = type->named_type();
  if (nt == NULL)
    nt = type->deref()->named_type();
  if (nt != NULL)
    {
      if (nt->is_unexported_local_method(gogo, name))
	return true;

      for (std::vector<const Named_type*>::const_iterator p = seen->begin();
	   p != seen->end();
	   ++p)
	{
	  if (*p == nt)
	    {
	      // We've already seen this type.
	      return false;
	    }
	}
    }

  type = type->deref();

  const Interface_type* it = type->interface_type();
  if (it != NULL && it->is_unexported_method(gogo, name))
    return true;

  const Struct_type* st = type->struct_type();
  if (st != NULL && st->is_unexported_local_field(gogo, name))
    return true;

  if (st == NULL)
    return false;

  const Struct_field_list* fields = st->fields();
  if (fields == NULL)
    return false;

  if (nt != NULL)
    seen->push_back(nt);

  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf)
    {
      if (pf->is_anonymous()
	  && !pf->type()->deref()->is_error_type()
	  && !pf->type()->deref()->is_undefined())
	{
	  Named_type* subtype = pf->type()->named_type();
	  if (subtype == NULL)
	    subtype = pf->type()->deref()->named_type();
	  if (subtype == NULL)
	    {
	      // This is an error, but it will be diagnosed elsewhere.
	      continue;
	    }
	  if (Type::is_unexported_field_or_method(gogo, subtype, name, seen))
	    {
	      if (nt != NULL)
		seen->pop_back();
	      return true;
	    }
	}
    }

  if (nt != NULL)
    seen->pop_back();

  return false;
}

// Class Forward_declaration.

Forward_declaration_type::Forward_declaration_type(Named_object* named_object)
  : Type(TYPE_FORWARD),
    named_object_(named_object->resolve()), warned_(false)
{
  gcc_assert(this->named_object_->is_unknown()
	     || this->named_object_->is_type_declaration());
}

// Return the named object.

Named_object*
Forward_declaration_type::named_object()
{
  return this->named_object_->resolve();
}

const Named_object*
Forward_declaration_type::named_object() const
{
  return this->named_object_->resolve();
}

// Return the name of the forward declared type.

const std::string&
Forward_declaration_type::name() const
{
  return this->named_object()->name();
}

// Warn about a use of a type which has been declared but not defined.

void
Forward_declaration_type::warn() const
{
  Named_object* no = this->named_object_->resolve();
  if (no->is_unknown())
    {
      // The name was not defined anywhere.
      if (!this->warned_)
	{
	  error_at(this->named_object_->location(),
		   "use of undefined type %qs",
		   no->message_name().c_str());
	  this->warned_ = true;
	}
    }
  else if (no->is_type_declaration())
    {
      // The name was seen as a type, but the type was never defined.
      if (no->type_declaration_value()->using_type())
	{
	  error_at(this->named_object_->location(),
		   "use of undefined type %qs",
		   no->message_name().c_str());
	  this->warned_ = true;
	}
    }
  else
    {
      // The name was defined, but not as a type.
      if (!this->warned_)
	{
	  error_at(this->named_object_->location(), "expected type");
	  this->warned_ = true;
	}
    }
}

// Get the base type of a declaration.  This gives an error if the
// type has not yet been defined.

Type*
Forward_declaration_type::real_type()
{
  if (this->is_defined())
    return this->named_object()->type_value();
  else
    {
      this->warn();
      return Type::make_error_type();
    }
}

const Type*
Forward_declaration_type::real_type() const
{
  if (this->is_defined())
    return this->named_object()->type_value();
  else
    {
      this->warn();
      return Type::make_error_type();
    }
}

// Return whether the base type is defined.

bool
Forward_declaration_type::is_defined() const
{
  return this->named_object()->is_type();
}

// Add a method.  This is used when methods are defined before the
// type.

Named_object*
Forward_declaration_type::add_method(const std::string& name,
				     Function* function)
{
  Named_object* no = this->named_object();
  if (no->is_unknown())
    no->declare_as_type();
  return no->type_declaration_value()->add_method(name, function);
}

// Add a method declaration.  This is used when methods are declared
// before the type.

Named_object*
Forward_declaration_type::add_method_declaration(const std::string& name,
						 Function_type* type,
						 source_location location)
{
  Named_object* no = this->named_object();
  if (no->is_unknown())
    no->declare_as_type();
  Type_declaration* td = no->type_declaration_value();
  return td->add_method_declaration(name, type, location);
}

// Traversal.

int
Forward_declaration_type::do_traverse(Traverse* traverse)
{
  if (this->is_defined()
      && Type::traverse(this->real_type(), traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Get a tree for the type.

tree
Forward_declaration_type::do_get_tree(Gogo* gogo)
{
  if (this->is_defined())
    return Type::get_named_type_tree(gogo, this->real_type());

  if (this->warned_)
    return error_mark_node;

  // We represent an undefined type as a struct with no fields.  That
  // should work fine for the middle-end, since the same case can
  // arise in C.
  Named_object* no = this->named_object();
  tree type_tree = make_node(RECORD_TYPE);
  tree id = no->get_id(gogo);
  tree decl = build_decl(no->location(), TYPE_DECL, id, type_tree);
  TYPE_NAME(type_tree) = decl;
  layout_type(type_tree);
  return type_tree;
}

// Build a type descriptor for a forwarded type.

Expression*
Forward_declaration_type::do_type_descriptor(Gogo* gogo, Named_type* name)
{
  if (!this->is_defined())
    return Expression::make_nil(BUILTINS_LOCATION);
  else
    {
      Type* t = this->real_type();
      if (name != NULL)
	return this->named_type_descriptor(gogo, t, name);
      else
	return Expression::make_type_descriptor(t, BUILTINS_LOCATION);
    }
}

// The reflection string.

void
Forward_declaration_type::do_reflection(Gogo* gogo, std::string* ret) const
{
  this->append_reflection(this->real_type(), gogo, ret);
}

// The mangled name.

void
Forward_declaration_type::do_mangled_name(Gogo* gogo, std::string* ret) const
{
  if (this->is_defined())
    this->append_mangled_name(this->real_type(), gogo, ret);
  else
    {
      const Named_object* no = this->named_object();
      std::string name;
      if (no->package() == NULL)
	name = gogo->package_name();
      else
	name = no->package()->name();
      name += '.';
      name += Gogo::unpack_hidden_name(no->name());
      char buf[20];
      snprintf(buf, sizeof buf, "N%u_",
	       static_cast<unsigned int>(name.length()));
      ret->append(buf);
      ret->append(name);
    }
}

// Export a forward declaration.  This can happen when a defined type
// refers to a type which is only declared (and is presumably defined
// in some other file in the same package).

void
Forward_declaration_type::do_export(Export*) const
{
  // If there is a base type, that should be exported instead of this.
  gcc_assert(!this->is_defined());

  // We don't output anything.
}

// Make a forward declaration.

Type*
Type::make_forward_declaration(Named_object* named_object)
{
  return new Forward_declaration_type(named_object);
}

// Class Typed_identifier_list.

// Sort the entries by name.

struct Typed_identifier_list_sort
{
 public:
  bool
  operator()(const Typed_identifier& t1, const Typed_identifier& t2) const
  { return t1.name() < t2.name(); }
};

void
Typed_identifier_list::sort_by_name()
{
  std::sort(this->entries_.begin(), this->entries_.end(),
	    Typed_identifier_list_sort());
}

// Traverse types.

int
Typed_identifier_list::traverse(Traverse* traverse)
{
  for (Typed_identifier_list::const_iterator p = this->begin();
       p != this->end();
       ++p)
    {
      if (Type::traverse(p->type(), traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

// Copy the list.

Typed_identifier_list*
Typed_identifier_list::copy() const
{
  Typed_identifier_list* ret = new Typed_identifier_list();
  for (Typed_identifier_list::const_iterator p = this->begin();
       p != this->end();
       ++p)
    ret->push_back(Typed_identifier(p->name(), p->type(), p->location()));
  return ret;
}
