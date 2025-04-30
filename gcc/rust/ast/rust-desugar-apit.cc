// Copyright (C) 2025 Free Software Foundation, Inc.

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

#include "rust-desugar-apit.h"
#include "rust-ast.h"
#include "rust-type.h"

namespace Rust {
namespace AST {

class DesugarApitType : public DefaultASTVisitor
{
  using DefaultASTVisitor::visit;

public:
  static std::pair<AST::Type *, std::vector<std::unique_ptr<GenericParam>>>
  Desugar (AST::Type &type)
  {
    DesugarApitType visitor (&type);
    type.accept_vis (visitor);
    rust_assert (visitor.translated != nullptr);
    return std::make_pair (visitor.translated,
			   std::move (visitor.implicit_generic_params));
  }

  // Generate a unique impl trait parameter name
  static Identifier get_impl_name ()
  {
    static size_t counter = 0;
    return Identifier ("Impl_" + std::to_string (counter++));
  }

  // these can hold other types
  void visit (AST::TupleType &tuple) override
  {
    for (auto &elem : tuple.get_elems ())
      {
	auto &type = *elem.get ();
	auto desugar = Desugar (type);
	auto tt = desugar.first;

	auto &implicit_generics = desugar.second;
	if (implicit_generics.empty ())
	  continue;

	if (tt != elem.get ())
	  elem = std::unique_ptr<Type> (tt);

	for (auto &implicit_generic : implicit_generics)
	  implicit_generic_params.push_back (std::move (implicit_generic));
      }
  }

  void visit (AST::ArrayType &type) override
  {
    auto &element_type = type.get_element_type ();
    auto desugar = Desugar (*element_type);
    auto tt = desugar.first;

    auto &implicit_generics = desugar.second;
    if (implicit_generics.empty ())
      return;

    if (tt != element_type.get ())
      element_type = std::unique_ptr<AST::Type> (tt);

    for (auto &implicit_generic : implicit_generics)
      implicit_generic_params.push_back (std::move (implicit_generic));
  }

  void visit (AST::ReferenceType &type) override
  {
    // Get a reference to the current type for in-place modification
    auto &referenced_type = type.get_type_referenced ();
    auto desugar = Desugar (referenced_type);
    auto tt = desugar.first;

    auto &implicit_generics = desugar.second;
    if (implicit_generics.empty ())
      return;

    // Update the reference type's contents rather than creating a new one
    if (&referenced_type != tt)
      {
	std::unique_ptr<AST::TypeNoBounds> new_type_no_bounds (
	  static_cast<AST::TypeNoBounds *> (tt));
	type.get_type_ptr () = std::move (new_type_no_bounds);
      }

    // Collect all the implicit generic parameters we found
    for (auto &implicit_generic : implicit_generics)
      implicit_generic_params.push_back (std::move (implicit_generic));
  }

  void visit (AST::RawPointerType &type) override
  {
    auto &pointed_type = type.get_type_pointed_to ();
    auto desugar = Desugar (pointed_type);
    auto tt = desugar.first;

    auto &implicit_generics = desugar.second;
    if (implicit_generics.empty ())
      return;

    // Update the pointer's inner type directly using the new accessor
    if (&pointed_type != tt)
      {
	std::unique_ptr<AST::TypeNoBounds> new_type_no_bounds (
	  static_cast<AST::TypeNoBounds *> (tt));
	type.get_type_ptr () = std::move (new_type_no_bounds);
      }

    // Collect all the implicit generic parameters we found
    for (auto &implicit_generic : implicit_generics)
      implicit_generic_params.push_back (std::move (implicit_generic));
  }

  void visit (AST::SliceType &type) override
  {
    auto &element_type = type.get_elem_type ();
    auto desugar = Desugar (element_type);
    auto tt = desugar.first;

    auto &implicit_generics = desugar.second;
    if (implicit_generics.empty ())
      return;

    if (&element_type != tt)
      {
	std::unique_ptr<AST::Type> new_elem_type (tt);
	type.get_elem_type_ptr () = std::move (new_elem_type);
      }

    // Collect all the implicit generic parameters we found
    for (auto &implicit_generic : implicit_generics)
      implicit_generic_params.push_back (std::move (implicit_generic));
  }

  void visit (AST::ParenthesisedType &type) override
  {
    auto &inner_type_ptr = type.get_type_in_parens ();
    auto desugar = Desugar (*inner_type_ptr);
    auto tt = desugar.first;

    auto &implicit_generics = desugar.second;
    if (implicit_generics.empty ())
      return;

    if (inner_type_ptr.get () != tt)
      {
	std::unique_ptr<AST::Type> new_inner_type (tt);
	inner_type_ptr = std::move (new_inner_type);
      }

    // Collect all the implicit generic parameters we found
    for (auto &implicit_generic : implicit_generics)
      implicit_generic_params.push_back (std::move (implicit_generic));
  }

  // this is where the desugar happens
  void visit (AST::ImplTraitType &type) override
  {
    // Generate a unique name using the static method
    auto ident = get_impl_name ();

    // Create a type path for the new generic parameter
    // Create a SimplePathSegment with the identifier string
    auto simple_seg = SimplePathSegment (ident.as_string (), type.get_locus ());
    // Create a vector of SimplePathSegments for SimplePath constructor
    std::vector<SimplePathSegment> simple_segs = {simple_seg};
    // Create a SimplePath
    auto simple_path = SimplePath (simple_segs, false, type.get_locus ());

    // Convert to TypePath by creating path segments
    std::vector<std::unique_ptr<TypePathSegment>> segments;
    segments.push_back (std::unique_ptr<TypePathSegment> (new TypePathSegment (
      PathIdentSegment (ident.as_string (), type.get_locus ()), false,
      type.get_locus ())));

    // Create TypePath from segments
    auto type_path
      = new TypePath (std::move (segments), type.get_locus (), false);

    // Convert bounds from impl trait to generic parameter bounds
    std::vector<std::unique_ptr<TypeParamBound>> bounds;
    for (auto &bound : type.get_type_param_bounds ())
      bounds.push_back (bound->clone_type_param_bound ());

    // Create the new generic parameter
    auto generic_param = std::unique_ptr<TypeParam> (
      new TypeParam (ident, type.get_locus (), std::move (bounds)));

    // Store the generic parameter to be added to the function signature
    implicit_generic_params.push_back (std::move (generic_param));

    // Replace impl trait with the new type parameter
    translated = type_path;
  }

  void visit (AST::ImplTraitTypeOneBound &type) override
  {
    // Generate a unique name using the static method
    auto ident = get_impl_name ();

    // Create a type path for the new generic parameter
    // Create a SimplePathSegment with the identifier string
    auto simple_seg = SimplePathSegment (ident.as_string (), type.get_locus ());
    // Create a vector of SimplePathSegments for SimplePath constructor
    std::vector<SimplePathSegment> simple_segs = {simple_seg};
    // Create a SimplePath
    auto simple_path = SimplePath (simple_segs, false, type.get_locus ());

    // Convert to TypePath by creating path segments
    std::vector<std::unique_ptr<TypePathSegment>> segments;
    segments.push_back (std::unique_ptr<TypePathSegment> (new TypePathSegment (
      PathIdentSegment (ident.as_string (), type.get_locus ()), false,
      type.get_locus ())));

    // Create TypePath from segments
    auto type_path
      = new TypePath (std::move (segments), type.get_locus (), false);

    // Convert the bound to a generic parameter bound
    std::vector<std::unique_ptr<TypeParamBound>> bounds;
    bounds.push_back (std::move (type.get_trait_bound ()));

    // Create the new generic parameter
    auto generic_param = std::unique_ptr<TypeParam> (
      new TypeParam (ident, type.get_locus (), std::move (bounds)));

    // Store the generic parameter to be added to the function signature
    implicit_generic_params.push_back (std::move (generic_param));

    // Replace impl trait with the new type parameter
    translated = type_path;
  }

private:
  DesugarApitType (AST::Type *base)
    : translated (base), implicit_generic_params ()
  {}

  AST::Type *translated;
  std::vector<std::unique_ptr<GenericParam>> implicit_generic_params;
};

// ---------

class ApitBoundProcessor
{
public:
  ApitBoundProcessor (
    WhereClause &where_clause,
    std::vector<std::unique_ptr<GenericParam>> &generic_params)
    : where_clause (where_clause), generic_params (generic_params)
  {}

  void go (std::vector<std::unique_ptr<GenericParam>> &implicit_generics)
  {
    // some desugars are more complex so imagine this case
    //
    // pub fn foo(_value: impl Bar<Baz = impl Foo>) -> i32 {
    //   15
    // }
    //
    // this needs to become:
    //
    // pub fn foo<T, U>(_value: T) -> i32
    // where
    //     T: Bar<Baz = U>,
    //     U: Foo,
    // {
    //     15
    // }
    //
    // so we need to walk all the implicit generics and the trait bounds paths
    // for more generics

    for (auto &implicit_generic : implicit_generics)
      {
	switch (implicit_generic->get_kind ())
	  {
	    case GenericParam::Kind::Type: {
	      TypeParam &p
		= *static_cast<TypeParam *> (implicit_generic.get ());

	      process_type_param (p);
	      generic_params.push_back (std::move (implicit_generic));
	      for (auto &synth : synthetic_params)
		generic_params.push_back (std::move (synth));
	      synthetic_params.clear ();
	    }
	    break;

	  default:
	    generic_params.push_back (std::move (implicit_generic));
	    break;
	  }
      }
  }

private:
  void process_type_param (TypeParam &p)
  {
    auto &bounds = p.get_type_param_bounds ();
    std::vector<size_t> bounds_to_remove;
    for (size_t i = 0; i < bounds.size (); i++)
      {
	auto &tb = bounds[i];
	switch (tb->get_bound_type ())
	  {
	    case TypeParamBound::TypeParamBoundType::TRAIT: {
	      TraitBound &ttb = *static_cast<TraitBound *> (tb.get ());
	      TypePath &path = ttb.get_type_path ();
	      bool deusgared = process_type_path (p, ttb, path);
	      if (deusgared)
		bounds_to_remove.push_back (i);
	    }

	  default:
	    break;
	  }
      }
    for (auto it = bounds_to_remove.rbegin (); it != bounds_to_remove.rend ();
	 ++it)
      bounds.erase (bounds.begin () + *it);
  }

  bool process_type_path (TypeParam &p, TraitBound &parent, TypePath &path)
  {
    bool desugared = false;
    for (auto &segment : path.get_segments ())
      {
	switch (segment->get_type ())
	  {
	    case TypePathSegment::SegmentType::GENERIC: {
	      TypePathSegmentGeneric &seg
		= *static_cast<TypePathSegmentGeneric *> (segment.get ());
	      desugared |= process_generic_segment (p, parent, path, seg);
	    }

	  default:
	    break;
	  }
      }
    return desugared;
  }

  bool process_generic_segment (TypeParam &p, TraitBound &parent,
				TypePath &path, TypePathSegmentGeneric &seg)
  {
    // we need to look for any impl types as default arguments in any generics
    // and remove this index from the generic arguments by using a where
    // constraint instead

    std::vector<std::unique_ptr<WhereClauseItem>> new_clauses;
    GenericArgs &generic_args = seg.get_generic_args ();
    std::vector<std::reference_wrapper<const GenericArgsBinding>>
      bindings_desugared;
    std::vector<GenericArgsBinding> &bindings
      = generic_args.get_binding_args ();

    for (auto &generic : bindings)
      {
	auto &t = generic.get_type ();
	auto translated = DesugarApitType::Desugar (t);
	auto tt = translated.first;

	auto &implicit_generics = translated.second;
	if (implicit_generics.empty ())
	  continue;

	if (tt != &t)
	  {
	    bindings_desugared.push_back (generic);
	    generic.get_type_ptr () = std::unique_ptr<Type> (tt);
	  }

	for (auto &implicit_generic : implicit_generics)
	  {
	    switch (implicit_generic->get_kind ())
	      {
		case GenericParam::Kind::Type: {
		  TypeParam &tp
		    = *static_cast<TypeParam *> (implicit_generic.get ());

		  std::vector<std::unique_ptr<TypeParamBound>>
		    type_param_bounds;
		  for (auto &b : tp.get_type_param_bounds ())
		    type_param_bounds.push_back (std::move (b));
		  tp.get_type_param_bounds ().clear ();

		  // add synthetic parameter for this
		  synthetic_params.push_back (std::move (implicit_generic));

		  auto bound_type_path
		    = get_type_for_identifier (tp.get_type_representation ());

		  auto clause = new TypeBoundWhereClauseItem (
		    {}, std::move (bound_type_path),
		    std::move (type_param_bounds), tp.get_locus ());
		  std::unique_ptr<WhereClauseItem> clause_item
		    = std::unique_ptr<WhereClauseItem> (clause);
		  new_clauses.push_back (std::move (clause_item));
		}
		break;

	      default:
		synthetic_params.push_back (std::move (implicit_generic));
		break;
	      }
	  }
      }

    std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds;
    auto bound = std::unique_ptr<TypeParamBound> (new TraitBound (parent));
    type_param_bounds.push_back (std::move (bound));
    auto parent_type_path
      = get_type_for_identifier (p.get_type_representation ());
    auto clause
      = new TypeBoundWhereClauseItem ({}, std::move (parent_type_path),
				      std::move (type_param_bounds),
				      parent.get_locus ());
    std::unique_ptr<WhereClauseItem> clause_item
      = std::unique_ptr<WhereClauseItem> (clause);
    where_clause.get_items ().push_back (std::move (clause_item));

    for (auto &where_item : new_clauses)
      where_clause.get_items ().push_back (std::move (where_item));

    return !bindings_desugared.empty ();
  }

  static std::unique_ptr<Type> get_type_for_identifier (const Identifier &ident)
  {
    auto simple_seg
      = SimplePathSegment (ident.as_string (), ident.get_locus ());
    std::vector<SimplePathSegment> simple_segs = {simple_seg};
    auto simple_path = SimplePath (simple_segs, false, ident.get_locus ());
    std::vector<std::unique_ptr<TypePathSegment>> segments;
    segments.push_back (std::unique_ptr<TypePathSegment> (new TypePathSegment (
      PathIdentSegment (ident.as_string (), ident.get_locus ()), false,
      ident.get_locus ())));
    auto type_path = new TypePath (std::move (segments), ident.get_locus ());
    return std::unique_ptr<Type> (type_path);
  }

private:
  WhereClause &where_clause;
  std::vector<std::unique_ptr<GenericParam>> &generic_params;

  // mutates
  std::vector<std::unique_ptr<GenericParam>> synthetic_params;
};

// ---------

DesugarApit::DesugarApit () {}

void
DesugarApit::go (AST::Crate &crate)
{
  DefaultASTVisitor::visit (crate);
}

void
DesugarApit::visit (AST::Function &function)
{
  if (!function.has_function_params ())
    return;

  auto &fn_params = function.get_function_params ();
  for (auto &param : fn_params)
    {
      if (param->is_variadic () || param->is_self ())
	continue;

      auto *p = param.get ();
      auto &fp = *static_cast<AST::FunctionParam *> (p);
      auto &type = fp.get_type ();

      auto translated = DesugarApitType::Desugar (type);
      auto tt = translated.first;

      auto &implicit_generics = translated.second;
      if (implicit_generics.empty ())
	continue;

      if (fp.get_type_ptr ().get () != tt)
	{
	  fp.get_type_ptr () = std::unique_ptr<AST::Type> (tt);
	}

      ApitBoundProcessor processor (function.get_where_clause (),
				    function.get_generic_params ());
      processor.go (implicit_generics);
    }
}

} // namespace AST
} // namespace Rust
