// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#ifndef RUST_TYTY_SUBST_H
#define RUST_TYTY_SUBST_H

#include "rust-system.h"
#include "rust-location.h"
#include "rust-hir-full-decls.h"
#include "rust-tyty-bounds.h"

namespace Rust {
namespace TyTy {

class BaseType;
class ParamType;
class SubstitutionArgumentMappings;
class SubstitutionParamMapping
{
public:
  SubstitutionParamMapping (const HIR::TypeParam &generic, ParamType *param);

  SubstitutionParamMapping (const SubstitutionParamMapping &other);

  std::string as_string () const;

  bool fill_param_ty (SubstitutionArgumentMappings &subst_mappings,
		      Location locus);

  SubstitutionParamMapping clone () const;

  ParamType *get_param_ty ();

  const ParamType *get_param_ty () const;

  const HIR::TypeParam &get_generic_param () const;

  // this is used for the backend to override the HirId ref of the param to
  // what the concrete type is for the rest of the context
  void override_context ();

  bool needs_substitution () const;

  Location get_param_locus () const;

  bool param_has_default_ty () const;

  BaseType *get_default_ty () const;

  bool need_substitution () const;

private:
  const HIR::TypeParam &generic;
  ParamType *param;
};

class SubstitutionArg
{
public:
  SubstitutionArg (const SubstitutionParamMapping *param, BaseType *argument);

  // FIXME
  // the copy constructors need removed - they are unsafe see
  // TypeBoundPredicate
  SubstitutionArg (const SubstitutionArg &other);

  SubstitutionArg &operator= (const SubstitutionArg &other);

  BaseType *get_tyty ();

  const BaseType *get_tyty () const;

  const SubstitutionParamMapping *get_param_mapping () const;

  static SubstitutionArg error ();

  bool is_error () const;

  bool is_conrete () const;

  std::string as_string () const;

private:
  const SubstitutionParamMapping *param;
  BaseType *argument;
};

typedef std::function<void (const ParamType &, const SubstitutionArg &)>
  ParamSubstCb;
class SubstitutionArgumentMappings
{
public:
  SubstitutionArgumentMappings (std::vector<SubstitutionArg> mappings,
				std::map<std::string, BaseType *> binding_args,
				Location locus,
				ParamSubstCb param_subst_cb = nullptr,
				bool trait_item_flag = false);

  SubstitutionArgumentMappings (const SubstitutionArgumentMappings &other);
  SubstitutionArgumentMappings &
  operator= (const SubstitutionArgumentMappings &other);

  SubstitutionArgumentMappings (SubstitutionArgumentMappings &&other) = default;
  SubstitutionArgumentMappings &operator= (SubstitutionArgumentMappings &&other)
    = default;

  static SubstitutionArgumentMappings error ();

  bool is_error () const;

  bool get_argument_for_symbol (const ParamType *param_to_find,
				SubstitutionArg *argument);

  bool get_argument_at (size_t index, SubstitutionArg *argument);

  // is_concrete means if the used args is non error, ie: non empty this will
  // verify if actual real types have been put in place of are they still
  // ParamTy
  bool is_concrete () const;

  Location get_locus () const;

  size_t size () const;

  bool is_empty () const;

  std::vector<SubstitutionArg> &get_mappings ();

  const std::vector<SubstitutionArg> &get_mappings () const;

  std::map<std::string, BaseType *> &get_binding_args ();

  const std::map<std::string, BaseType *> &get_binding_args () const;

  std::string as_string () const;

  void on_param_subst (const ParamType &p, const SubstitutionArg &a) const;

  ParamSubstCb get_subst_cb () const;

  bool trait_item_mode () const;

private:
  std::vector<SubstitutionArg> mappings;
  std::map<std::string, BaseType *> binding_args;
  Location locus;
  ParamSubstCb param_subst_cb;
  bool trait_item_flag;
};

class SubstitutionRef
{
public:
  SubstitutionRef (std::vector<SubstitutionParamMapping> substitutions,
		   SubstitutionArgumentMappings arguments);

  bool has_substitutions () const;

  std::string subst_as_string () const;

  bool supports_associated_bindings () const;

  // this is overridden in TypeBoundPredicate
  // which support bindings we don't add them directly to the SubstitutionRef
  // base class because this class represents the fn<X: Foo, Y: Bar>. The only
  // construct which supports associated types
  virtual size_t get_num_associated_bindings () const;

  // this is overridden in TypeBoundPredicate
  virtual TypeBoundPredicateItem
  lookup_associated_type (const std::string &search);

  size_t get_num_substitutions () const;

  std::vector<SubstitutionParamMapping> &get_substs ();

  const std::vector<SubstitutionParamMapping> &get_substs () const;

  std::vector<SubstitutionParamMapping> clone_substs () const;

  void override_context ();

  bool needs_substitution () const;

  bool was_substituted () const;

  SubstitutionArgumentMappings &get_substitution_arguments ();
  const SubstitutionArgumentMappings &get_substitution_arguments () const;

  // this is the count of type params that are not substituted fuly
  size_t num_required_substitutions () const;

  // this is the count of type params that need substituted taking into account
  // possible defaults
  size_t min_required_substitutions () const;

  // We are trying to subst <i32, f32> into Struct Foo<X,Y> {}
  // in the case of Foo<i32,f32>{...}
  //
  // the substitions we have here define X,Y but the arguments have no bindings
  // so its a matter of ordering
  SubstitutionArgumentMappings
  get_mappings_from_generic_args (HIR::GenericArgs &args);

  // Recursive substitutions
  // Foo <A,B> { a:A, b: B}; Bar <X,Y,Z>{a:X, b: Foo<Y,Z>}
  //
  // we have bindings for X Y Z and need to propagate the binding Y,Z into Foo
  // Which binds to A,B
  SubstitutionArgumentMappings
  adjust_mappings_for_this (SubstitutionArgumentMappings &mappings);

  // Are the mappings here actually bound to this type. For example imagine the
  // case:
  //
  // struct Foo<T>(T);
  // impl<T> Foo<T> {
  //   fn test(self) { ... }
  // }
  //
  // In this case we have a generic ADT of Foo and an impl block of a generic T
  // on Foo for the Self type. When we it comes to path resolution we can have:
  //
  // Foo::<i32>::test()
  //
  // This means the first segment of Foo::<i32> returns the ADT Foo<i32> not the
  // Self ADT bound to the T from the impl block. This means when it comes to
  // the next segment of test which resolves to the function we need to check
  // wether the arguments in the struct definition of foo can be bound here
  // before substituting the previous segments type here. This functions acts as
  // a guard for the solve_mappings_from_receiver_for_self to handle the case
  // where arguments are not bound. This is important for this next case:
  //
  // struct Baz<A, B>(A, B);
  // impl Baz<i32, f32> {
  //   fn test<X>(a: X) -> X {
  //       a
  //   }
  // }
  //
  // In this case Baz has been already substituted for the impl's Self to become
  // ADT<i32, f32> so that the function test only has 1 generic argument of X.
  // The path for this will be:
  //
  // Baz::test::<_>(123)
  //
  // So the first segment here will be Baz<_, _> to try and infer the arguments
  // which will be taken from the impl's Self type in this case since it is
  // already substituted and like the previous case the check to see if we need
  // to inherit the previous segments generic arguments takes place but the
  // generic arguments are not bound to this type as they have already been
  // substituted.
  //
  // Its important to remember from the first example the FnType actually looks
  // like:
  //
  // fn <T>test(self :Foo<T>(T))
  //
  // As the generic parameters are "bound" to each of the items in the impl
  // block. So this check is about wether the arguments we have here can
  // actually be bound to this type.
  bool are_mappings_bound (SubstitutionArgumentMappings &mappings);

  // struct Foo<A, B>(A, B);
  //
  // impl<T> Foo<T, f32>;
  //     -> fn test<X>(self, a: X) -> X
  //
  // We might invoke this via:
  //
  // a = Foo(123, 456f32);
  // b = a.test::<bool>(false);
  //
  // we need to figure out relevant generic arguemts for self to apply to the
  // fntype
  SubstitutionArgumentMappings solve_mappings_from_receiver_for_self (
    SubstitutionArgumentMappings &mappings) const;

  // TODO comment
  SubstitutionArgumentMappings
  solve_missing_mappings_from_this (SubstitutionRef &ref, SubstitutionRef &to);

  // TODO comment
  BaseType *infer_substitions (Location locus);

  // this clears any possible projections from higher ranked trait bounds which
  // could be hanging around from a previous resolution
  void prepare_higher_ranked_bounds ();

  // FIXME
  // this is bad name for this, i think it should be something like
  // compute-higher-ranked-bounds
  bool monomorphize ();

  // TODO comment
  virtual BaseType *handle_substitions (SubstitutionArgumentMappings &mappings)
    = 0;

  SubstitutionArgumentMappings get_used_arguments () const;

protected:
  Resolver::AssociatedImplTrait *lookup_associated_impl (
    const SubstitutionParamMapping &subst, const TypeBoundPredicate &bound,
    const TyTy::BaseType *binding, bool *error_flag) const;

  std::vector<SubstitutionParamMapping> substitutions;
  SubstitutionArgumentMappings used_arguments;
};

} // namespace TyTy
} // namespace Rust
#endif // RUST_TYTY_SUBST_H
