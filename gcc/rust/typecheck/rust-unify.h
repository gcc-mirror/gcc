// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#ifndef RUST_UNIFY
#define RUST_UNIFY

#include "rust-tyty-util.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace Resolver {

class UnifyRules
{
public:
  struct InferenceSite
  {
    HirId pref;
    HirId ptyref;
    TyTy::ParamType *param;
    TyTy::InferType *infer;
  };
  struct CommitSite
  {
    TyTy::BaseType *lhs;
    TyTy::BaseType *rhs;
    TyTy::BaseType *resolved;
  };

  static TyTy::BaseType *Resolve (TyTy::TyWithLocation lhs,
				  TyTy::TyWithLocation rhs, location_t locus,
				  bool commit_flag, bool emit_error, bool infer,
				  std::vector<CommitSite> &commits,
				  std::vector<InferenceSite> &infers);

  static void commit (TyTy::BaseType *base, TyTy::BaseType *other,
		      TyTy::BaseType *resolved);

protected:
  TyTy::BaseType *expect_inference_variable (TyTy::InferType *ltype,
					     TyTy::BaseType *rtype);
  TyTy::BaseType *expect_adt (TyTy::ADTType *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_str (TyTy::StrType *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_reference (TyTy::ReferenceType *ltype,
				    TyTy::BaseType *rtype);
  TyTy::BaseType *expect_pointer (TyTy::PointerType *ltype,
				  TyTy::BaseType *rtype);
  TyTy::BaseType *expect_param (TyTy::ParamType *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_array (TyTy::ArrayType *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_slice (TyTy::SliceType *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_fndef (TyTy::FnType *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_fnptr (TyTy::FnPtr *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_tuple (TyTy::TupleType *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_bool (TyTy::BoolType *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_char (TyTy::CharType *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_int (TyTy::IntType *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_uint (TyTy::UintType *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_float (TyTy::FloatType *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_isize (TyTy::ISizeType *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_usize (TyTy::USizeType *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_never (TyTy::NeverType *ltype, TyTy::BaseType *rtype);
  TyTy::BaseType *expect_placeholder (TyTy::PlaceholderType *ltype,
				      TyTy::BaseType *rtype);
  TyTy::BaseType *expect_projection (TyTy::ProjectionType *ltype,
				     TyTy::BaseType *rtype);
  TyTy::BaseType *expect_dyn (TyTy::DynamicObjectType *ltype,
			      TyTy::BaseType *rtype);
  TyTy::BaseType *expect_closure (TyTy::ClosureType *ltype,
				  TyTy::BaseType *rtype);

private:
  UnifyRules (TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
	      location_t locus, bool commit_flag, bool emit_error, bool infer,
	      std::vector<CommitSite> &commits,
	      std::vector<InferenceSite> &infers);

  void emit_type_mismatch () const;
  void emit_abi_mismatch (const TyTy::FnType &expected,
			  const TyTy::FnType &got) const;

  TyTy::BaseType *go ();

  TyTy::BaseType *get_base ();
  TyTy::BaseType *get_other ();

  TyTy::TyWithLocation lhs;
  TyTy::TyWithLocation rhs;
  location_t locus;
  bool commit_flag;
  bool emit_error;
  bool infer_flag;
  std::vector<CommitSite> &commits;
  std::vector<InferenceSite> &infers;

  Analysis::Mappings &mappings;
  TypeCheckContext &context;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_UNIFY
