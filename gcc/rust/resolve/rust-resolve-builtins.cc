// Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

#include "rust-resolve-builtins.h"
#include "rust-name-resolution-context.h"
#include "rust-tyty.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace Resolver2_0 {
namespace Builtins {

// Use X-macros

#define TYPE_UINT(n, enum_ident) TYPE1 (n, UintType, UintType::enum_ident)
#define TYPE_INT(n, enum_ident) TYPE1 (n, IntType, IntType::enum_ident)

#define BUILTIN_TYPES                                                          \
  TYPE0 ("bool", BoolType)                                                     \
  TYPE_UINT ("u8", U8)                                                         \
  TYPE_UINT ("u16", U16)                                                       \
  TYPE_UINT ("u32", U32)                                                       \
  TYPE_UINT ("u64", U64)                                                       \
  TYPE_UINT ("u128", U128)                                                     \
  TYPE_INT ("i8", I8)                                                          \
  TYPE_INT ("i16", I16)                                                        \
  TYPE_INT ("i32", I32)                                                        \
  TYPE_INT ("i64", I64)                                                        \
  TYPE_INT ("i128", I128)                                                      \
  TYPE1 ("f32", FloatType, FloatType::F32)                                     \
  TYPE1 ("f64", FloatType, FloatType::F64)                                     \
  TYPE0 ("usize", USizeType)                                                   \
  TYPE0 ("isize", ISizeType)                                                   \
  TYPE0 ("char", CharType)                                                     \
  TYPE0 ("str", StrType)                                                       \
  TYPE0 ("!", NeverType)

// Define constants using X macros

#define TYPE0(...) 1 +
#define TYPE1(...) 1 +
static constexpr size_t builtin_count = BUILTIN_TYPES 0;
#undef TYPE0
#undef TYPE1

#define TYPE0(n, ...) n,
#define TYPE1(n, ...) n,
static constexpr const char *builtin_names[] = {BUILTIN_TYPES};
#undef TYPE0
#undef TYPE1

static NodeId builtin_node_ids[builtin_count];

void
setup_lang_prelude (NameResolutionContext &ctx)
{
  auto &mappings = Analysis::Mappings::get ();

  // insert into prelude rib
  ctx.scoped (Rib::Kind::Prelude, 0, [&mappings, &ctx] (void) -> void {
    for (size_t i = 0; i < builtin_count; i++)
      {
	NodeId node_id = mappings.get_next_node_id ();
	rust_assert (ctx.types.insert (Identifier (builtin_names[i]), node_id));
	builtin_node_ids[i] = node_id;
      }
  });
}

void
setup_type_ctx ()
{
  auto &mappings = Analysis::Mappings::get ();
  auto &ty_ctx = *Resolver::TypeCheckContext::get ();

  HirId hir_ids[builtin_count];
  for (size_t i = 0; i < builtin_count; i++)
    hir_ids[i] = mappings.get_next_hir_id ();

  TyTy::BaseType *types[builtin_count];
  {
    size_t i = 0;
#define TYPE_BASE(stub)                                                        \
  types[i] = new TyTy::stub;                                                   \
  i++;
#define TYPE0(n, ty) TYPE_BASE (ty (hir_ids[i]))
#define TYPE1(n, ty, p1) TYPE_BASE (ty (hir_ids[i], TyTy::p1))
    BUILTIN_TYPES
#undef TYPE_BASE
#undef TYPE0
#undef TYPE1
  }

  for (size_t i = 0; i < builtin_count; i++)
    {
      NodeId node_id = builtin_node_ids[i];
      HirId hir_id = hir_ids[i];
      mappings.insert_node_to_hir (node_id, hir_id);
      ty_ctx.insert_builtin (hir_id, node_id, types[i]);
    }

  // handle unit type separately
  auto *unit_type = TyTy::TupleType::get_unit_type ();
  ty_ctx.insert_builtin (unit_type->get_ref (), mappings.get_next_node_id (),
			 unit_type);
}

} // namespace Builtins
} // namespace Resolver2_0
} // namespace Rust
