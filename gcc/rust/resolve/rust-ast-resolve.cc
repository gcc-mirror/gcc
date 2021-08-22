// Copyright (C) 2020 Free Software Foundation, Inc.

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

#include "rust-ast-resolve.h"
#include "rust-ast-full.h"
#include "rust-tyty.h"
#include "rust-ast-resolve-toplevel.h"
#include "rust-ast-resolve-item.h"
#include "rust-ast-resolve-expr.h"

#define MKBUILTIN_TYPE(_X, _R, _TY)                                            \
  do                                                                           \
    {                                                                          \
      AST::PathIdentSegment seg (_X);                                          \
      auto typePath = ::std::unique_ptr<AST::TypePathSegment> (                \
	new AST::TypePathSegment (::std::move (seg), false,                    \
				  Linemap::predeclared_location ()));          \
      ::std::vector< ::std::unique_ptr<AST::TypePathSegment> > segs;           \
      segs.push_back (::std::move (typePath));                                 \
      auto builtin_type                                                        \
	= new AST::TypePath (::std::move (segs),                               \
			     Linemap::predeclared_location (), false);         \
      _R.push_back (builtin_type);                                             \
      tyctx->insert_builtin (_TY->get_ref (), builtin_type->get_node_id (),    \
			     _TY);                                             \
    }                                                                          \
  while (0)

extern bool
saw_errors (void);

namespace Rust {
namespace Resolver {

// Resolver

Resolver::Resolver ()
  : mappings (Analysis::Mappings::get ()), tyctx (TypeCheckContext::get ()),
    name_scope (Scope (mappings->get_current_crate ())),
    type_scope (Scope (mappings->get_current_crate ())),
    label_scope (Scope (mappings->get_current_crate ())),
    global_type_node_id (UNKNOWN_NODEID), unit_ty_node_id (UNKNOWN_NODEID)
{
  generate_builtins ();
}

Resolver *
Resolver::get ()
{
  static Resolver *instance;
  if (instance == nullptr)
    instance = new Resolver ();

  return instance;
}

void
Resolver::push_new_name_rib (Rib *r)
{
  rust_assert (name_ribs.find (r->get_node_id ()) == name_ribs.end ());
  name_ribs[r->get_node_id ()] = r;
}

void
Resolver::push_new_type_rib (Rib *r)
{
  if (type_ribs.size () == 0)
    global_type_node_id = r->get_node_id ();

  rust_assert (type_ribs.find (r->get_node_id ()) == type_ribs.end ());
  type_ribs[r->get_node_id ()] = r;
}

void
Resolver::push_new_label_rib (Rib *r)
{
  rust_assert (label_ribs.find (r->get_node_id ()) == label_ribs.end ());
  label_ribs[r->get_node_id ()] = r;
}

bool
Resolver::find_name_rib (NodeId id, Rib **rib)
{
  auto it = name_ribs.find (id);
  if (it == name_ribs.end ())
    return false;

  *rib = it->second;
  return true;
}

bool
Resolver::find_type_rib (NodeId id, Rib **rib)
{
  auto it = type_ribs.find (id);
  if (it == type_ribs.end ())
    return false;

  *rib = it->second;
  return true;
}

void
Resolver::insert_builtin_types (Rib *r)
{
  auto builtins = get_builtin_types ();
  for (auto &builtin : builtins)
    {
      CanonicalPath builtin_path
	= CanonicalPath::new_seg (builtin->get_node_id (),
				  builtin->as_string ());
      r->insert_name (builtin_path, builtin->get_node_id (),
		      Linemap::predeclared_location (), false,
		      [] (const CanonicalPath &, NodeId, Location) -> void {});
    }
}

std::vector<AST::Type *> &
Resolver::get_builtin_types ()
{
  return builtins;
}

void
Resolver::generate_builtins ()
{
  auto u8
    = new TyTy::UintType (mappings->get_next_hir_id (), TyTy::UintType::U8);
  auto u16
    = new TyTy::UintType (mappings->get_next_hir_id (), TyTy::UintType::U16);
  auto u32
    = new TyTy::UintType (mappings->get_next_hir_id (), TyTy::UintType::U32);
  auto u64
    = new TyTy::UintType (mappings->get_next_hir_id (), TyTy::UintType::U64);
  auto u128
    = new TyTy::UintType (mappings->get_next_hir_id (), TyTy::UintType::U128);
  auto i8 = new TyTy::IntType (mappings->get_next_hir_id (), TyTy::IntType::I8);
  auto i16
    = new TyTy::IntType (mappings->get_next_hir_id (), TyTy::IntType::I16);
  auto i32
    = new TyTy::IntType (mappings->get_next_hir_id (), TyTy::IntType::I32);
  auto i64
    = new TyTy::IntType (mappings->get_next_hir_id (), TyTy::IntType::I64);
  auto i128
    = new TyTy::IntType (mappings->get_next_hir_id (), TyTy::IntType::I128);
  auto rbool = new TyTy::BoolType (mappings->get_next_hir_id ());
  auto f32
    = new TyTy::FloatType (mappings->get_next_hir_id (), TyTy::FloatType::F32);
  auto f64
    = new TyTy::FloatType (mappings->get_next_hir_id (), TyTy::FloatType::F64);
  auto usize = new TyTy::USizeType (mappings->get_next_hir_id ());
  auto isize = new TyTy::ISizeType (mappings->get_next_hir_id ());
  auto char_tyty = new TyTy::CharType (mappings->get_next_hir_id ());
  auto str = new TyTy::StrType (mappings->get_next_hir_id ());

  MKBUILTIN_TYPE ("u8", builtins, u8);
  MKBUILTIN_TYPE ("u16", builtins, u16);
  MKBUILTIN_TYPE ("u32", builtins, u32);
  MKBUILTIN_TYPE ("u64", builtins, u64);
  MKBUILTIN_TYPE ("u128", builtins, u128);
  MKBUILTIN_TYPE ("i8", builtins, i8);
  MKBUILTIN_TYPE ("i16", builtins, i16);
  MKBUILTIN_TYPE ("i32", builtins, i32);
  MKBUILTIN_TYPE ("i64", builtins, i64);
  MKBUILTIN_TYPE ("i128", builtins, i128);
  MKBUILTIN_TYPE ("bool", builtins, rbool);
  MKBUILTIN_TYPE ("f32", builtins, f32);
  MKBUILTIN_TYPE ("f64", builtins, f64);
  MKBUILTIN_TYPE ("usize", builtins, usize);
  MKBUILTIN_TYPE ("isize", builtins, isize);
  MKBUILTIN_TYPE ("char", builtins, char_tyty);
  MKBUILTIN_TYPE ("str", builtins, str);

  // unit type ()

  TyTy::TupleType *unit_tyty
    = new TyTy::TupleType (mappings->get_next_hir_id ());
  std::vector<std::unique_ptr<AST::Type> > elems;
  AST::TupleType *unit_type
    = new AST::TupleType (std::move (elems), Linemap::predeclared_location ());
  builtins.push_back (unit_type);
  tyctx->insert_builtin (unit_tyty->get_ref (), unit_type->get_node_id (),
			 unit_tyty);
  set_unit_type_node_id (unit_type->get_node_id ());
}

void
Resolver::insert_new_definition (NodeId id, Definition def)
{
  auto it = name_definitions.find (id);
  if (it != name_definitions.end ())
    {
      rust_assert (it->second.is_equal (def));
      return;
    }
  name_definitions[id] = def;
}

bool
Resolver::lookup_definition (NodeId id, Definition *def)
{
  auto it = name_definitions.find (id);
  if (it == name_definitions.end ())
    return false;

  *def = it->second;
  return true;
}

void
Resolver::insert_resolved_name (NodeId refId, NodeId defId)
{
  resolved_names[refId] = defId;
  get_name_scope ().append_reference_for_def (refId, defId);
}

bool
Resolver::lookup_resolved_name (NodeId refId, NodeId *defId)
{
  auto it = resolved_names.find (refId);
  if (it == resolved_names.end ())
    return false;

  *defId = it->second;
  return true;
}

void
Resolver::insert_resolved_type (NodeId refId, NodeId defId)
{
  auto it = resolved_types.find (refId);
  rust_assert (it == resolved_types.end ());

  resolved_types[refId] = defId;
  get_type_scope ().append_reference_for_def (refId, defId);
}

bool
Resolver::lookup_resolved_type (NodeId refId, NodeId *defId)
{
  auto it = resolved_types.find (refId);
  if (it == resolved_types.end ())
    return false;

  *defId = it->second;
  return true;
}

void
Resolver::insert_resolved_label (NodeId refId, NodeId defId)
{
  auto it = resolved_labels.find (refId);
  rust_assert (it == resolved_labels.end ());

  resolved_labels[refId] = defId;
  get_label_scope ().append_reference_for_def (refId, defId);
}

bool
Resolver::lookup_resolved_label (NodeId refId, NodeId *defId)
{
  auto it = resolved_labels.find (refId);
  if (it == resolved_labels.end ())
    return false;

  *defId = it->second;
  return true;
}

// NameResolution

NameResolution *
NameResolution::get ()
{
  static NameResolution *instance;
  if (instance == nullptr)
    instance = new NameResolution ();

  return instance;
}

NameResolution::NameResolution ()
  : resolver (Resolver::get ()), mappings (Analysis::Mappings::get ())
{
  // these are global
  resolver->get_type_scope ().push (mappings->get_next_node_id ());
  resolver->insert_builtin_types (resolver->get_type_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
}

void
NameResolution::Resolve (AST::Crate &crate)
{
  auto resolver = get ();
  resolver->go (crate);
}

void
NameResolution::go (AST::Crate &crate)
{
  // setup parent scoping for names
  resolver->get_name_scope ().push (crate.get_node_id ());
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  // setup parent scoping for new types
  resolver->get_type_scope ().push (mappings->get_next_node_id ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  // setup label scope
  resolver->get_label_scope ().push (mappings->get_next_node_id ());
  resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

  // first gather the top-level namespace names then we drill down
  for (auto it = crate.items.begin (); it != crate.items.end (); it++)
    ResolveTopLevel::go (it->get ());

  if (saw_errors ())
    return;

  // next we can drill down into the items and their scopes
  for (auto it = crate.items.begin (); it != crate.items.end (); it++)
    ResolveItem::go (it->get ());
}

// rust-ast-resolve-expr.h

void
ResolveExpr::visit (AST::BlockExpr &expr)
{
  NodeId scope_node_id = expr.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

  expr.iterate_stmts ([&] (AST::Stmt *s) mutable -> bool {
    ResolveStmt::go (s, s->get_node_id ());
    return true;
  });

  if (expr.has_tail_expr ())
    ResolveExpr::go (expr.get_tail_expr ().get (), expr.get_node_id ());

  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}

// rust-ast-resolve-struct-expr-field.h

void
ResolveStructExprField::visit (AST::StructExprFieldIdentifierValue &field)
{
  ResolveExpr::go (field.get_value ().get (), field.get_node_id ());
}

void
ResolveStructExprField::visit (AST::StructExprFieldIndexValue &field)
{
  ResolveExpr::go (field.get_value ().get (), field.get_node_id ());
}

void
ResolveStructExprField::visit (AST::StructExprFieldIdentifier &field)
{
  AST::IdentifierExpr expr (field.get_field_name (), {}, field.get_locus ());
  expr.set_node_id (field.get_node_id ());

  ResolveExpr::go (&expr, field.get_node_id ());
}

// rust-ast-resolve-type.h

std::string
ResolveTypeToCanonicalPath::canonicalize_generic_args (AST::GenericArgs &args)
{
  std::string buf;

  size_t i = 0;
  size_t total = args.get_type_args ().size ();

  for (auto &ty_arg : args.get_type_args ())
    {
      buf += ty_arg->as_string ();
      if ((i + 1) < total)
	buf += ",";

      i++;
    }

  return "<" + buf + ">";
}

bool
ResolveTypeToCanonicalPath::type_resolve_generic_args (AST::GenericArgs &args)
{
  for (auto &gt : args.get_type_args ())
    {
      ResolveType::go (gt.get (), UNKNOWN_NODEID);
      // FIXME error handling here for inference variable since they do not have
      // a node to resolve to
      // if (resolved == UNKNOWN_NODEID) return false;
    }
  return true;
}

void
ResolveTypeToCanonicalPath::visit (AST::TypePathSegmentGeneric &seg)
{
  if (seg.is_error ())
    {
      failure_flag = true;
      rust_error_at (seg.get_locus (), "segment has error: %s",
		     seg.as_string ().c_str ());
      return;
    }

  if (!seg.has_generic_args ())
    {
      result = CanonicalPath::new_seg (seg.get_node_id (),
				       seg.get_ident_segment ().as_string ());
      return;
    }

  if (type_resolve_generic_args_flag)
    {
      bool ok = type_resolve_generic_args (seg.get_generic_args ());
      failure_flag = !ok;
    }

  if (include_generic_args_flag)
    {
      std::string generics
	= canonicalize_generic_args (seg.get_generic_args ());
      result = CanonicalPath::new_seg (seg.get_node_id (),
				       seg.get_ident_segment ().as_string ()
					 + "::" + generics);
      return;
    }

  result = CanonicalPath::new_seg (seg.get_node_id (),
				   seg.get_ident_segment ().as_string ());
}

void
ResolveTypeToCanonicalPath::visit (AST::TypePathSegment &seg)
{
  if (seg.is_error ())
    {
      failure_flag = true;
      rust_error_at (seg.get_locus (), "segment has error: %s",
		     seg.as_string ().c_str ());
      return;
    }

  CanonicalPath ident_seg
    = CanonicalPath::new_seg (seg.get_node_id (),
			      seg.get_ident_segment ().as_string ());
  result = result.append (ident_seg);
}

// rust-ast-resolve-expr.h

void
ResolvePath::resolve_path (AST::PathInExpression *expr)
{
  // resolve root segment first then apply segments in turn
  std::vector<AST::PathExprSegment> &segs = expr->get_segments ();
  AST::PathExprSegment &root_segment = segs.at (0);
  AST::PathIdentSegment &root_ident_seg = root_segment.get_ident_segment ();

  bool segment_is_type = false;
  CanonicalPath root_seg_path
    = CanonicalPath::new_seg (root_segment.get_node_id (),
			      root_ident_seg.as_string ());

  // name scope first
  if (resolver->get_name_scope ().lookup (root_seg_path, &resolved_node))
    {
      segment_is_type = false;
      resolver->insert_resolved_name (root_segment.get_node_id (),
				      resolved_node);
      resolver->insert_new_definition (root_segment.get_node_id (),
				       Definition{expr->get_node_id (),
						  parent});
    }
  // check the type scope
  else if (resolver->get_type_scope ().lookup (root_seg_path, &resolved_node))
    {
      segment_is_type = true;
      resolver->insert_resolved_type (root_segment.get_node_id (),
				      resolved_node);
      resolver->insert_new_definition (root_segment.get_node_id (),
				       Definition{expr->get_node_id (),
						  parent});
    }
  else
    {
      rust_error_at (expr->get_locus (),
		     "Cannot find path %<%s%> in this scope",
		     root_segment.as_string ().c_str ());
      return;
    }

  if (root_segment.has_generic_args ())
    {
      bool ok = ResolveTypeToCanonicalPath::type_resolve_generic_args (
	root_segment.get_generic_args ());
      if (!ok)
	{
	  rust_error_at (root_segment.get_locus (),
			 "failed to resolve generic arguments");
	  return;
	}
    }

  bool is_single_segment = segs.size () == 1;
  if (is_single_segment)
    {
      if (segment_is_type)
	resolver->insert_resolved_type (expr->get_node_id (), resolved_node);
      else
	resolver->insert_resolved_name (expr->get_node_id (), resolved_node);

      resolver->insert_new_definition (expr->get_node_id (),
				       Definition{expr->get_node_id (),
						  parent});
      return;
    }

  resolve_segments (root_seg_path, 1, expr->get_segments (),
		    expr->get_node_id (), expr->get_locus ());
}

void
ResolvePath::resolve_path (AST::QualifiedPathInExpression *expr)
{
  AST::QualifiedPathType &root_segment = expr->get_qualified_path_type ();

  bool canonicalize_type_with_generics = false;
  ResolveType::go (&root_segment.get_as_type_path (),
		   root_segment.get_node_id (),
		   canonicalize_type_with_generics);

  ResolveType::go (root_segment.get_type ().get (), root_segment.get_node_id (),
		   canonicalize_type_with_generics);

  bool canonicalize_type_args = true;
  bool type_resolve_generic_args = true;

  CanonicalPath impl_type_seg
    = ResolveTypeToCanonicalPath::resolve (*root_segment.get_type ().get (),
					   canonicalize_type_args,
					   type_resolve_generic_args);

  CanonicalPath trait_type_seg
    = ResolveTypeToCanonicalPath::resolve (root_segment.get_as_type_path (),
					   canonicalize_type_args,
					   type_resolve_generic_args);
  CanonicalPath root_seg_path
    = TraitImplProjection::resolve (root_segment.get_node_id (), trait_type_seg,
				    impl_type_seg);
  bool segment_is_type = false;

  // name scope first
  if (resolver->get_name_scope ().lookup (root_seg_path, &resolved_node))
    {
      segment_is_type = false;
      resolver->insert_resolved_name (root_segment.get_node_id (),
				      resolved_node);
      resolver->insert_new_definition (root_segment.get_node_id (),
				       Definition{expr->get_node_id (),
						  parent});
    }
  // check the type scope
  else if (resolver->get_type_scope ().lookup (root_seg_path, &resolved_node))
    {
      segment_is_type = true;
      resolver->insert_resolved_type (root_segment.get_node_id (),
				      resolved_node);
      resolver->insert_new_definition (root_segment.get_node_id (),
				       Definition{expr->get_node_id (),
						  parent});
    }
  else
    {
      rust_error_at (expr->get_locus (),
		     "Cannot find path %<%s%> in this scope",
		     root_segment.as_string ().c_str ());
      return;
    }

  bool is_single_segment = expr->get_segments ().empty ();
  if (is_single_segment)
    {
      if (segment_is_type)
	resolver->insert_resolved_type (expr->get_node_id (), resolved_node);
      else
	resolver->insert_resolved_name (expr->get_node_id (), resolved_node);

      resolver->insert_new_definition (expr->get_node_id (),
				       Definition{expr->get_node_id (),
						  parent});
      return;
    }

  resolve_segments (root_seg_path, 0, expr->get_segments (),
		    expr->get_node_id (), expr->get_locus ());
}

void
ResolvePath::resolve_segments (CanonicalPath prefix, size_t offs,
			       std::vector<AST::PathExprSegment> &segs,
			       NodeId expr_node_id, Location expr_locus)
{
  // we can attempt to resolve this path fully
  CanonicalPath path = prefix;
  bool segment_is_type = false;
  for (size_t i = offs; i < segs.size (); i++)
    {
      AST::PathExprSegment &seg = segs.at (i);
      auto s = ResolvePathSegmentToCanonicalPath::resolve (seg);
      path = path.append (s);

      // reset state
      segment_is_type = false;
      resolved_node = UNKNOWN_NODEID;

      if (resolver->get_name_scope ().lookup (path, &resolved_node))
	{
	  resolver->insert_resolved_name (seg.get_node_id (), resolved_node);
	  resolver->insert_new_definition (seg.get_node_id (),
					   Definition{expr_node_id, parent});
	}
      // check the type scope
      else if (resolver->get_type_scope ().lookup (path, &resolved_node))
	{
	  segment_is_type = true;
	  resolver->insert_resolved_type (seg.get_node_id (), resolved_node);
	  resolver->insert_new_definition (seg.get_node_id (),
					   Definition{expr_node_id, parent});
	}
      else
	{
	  // attempt to fully resolve the path which is allowed to fail given
	  // the following scenario
	  //
	  // https://github.com/Rust-GCC/gccrs/issues/355 Paths are
	  // resolved fully here, there are limitations though imagine:
	  //
	  // struct Foo<A> (A);
	  //
	  // impl Foo<isize> {
	  //    fn test() -> ...
	  //
	  // impl Foo<f32> {
	  //    fn test() -> ...
	  //
	  // fn main() {
	  //    let a:i32 = Foo::test();
	  //
	  // there are multiple paths that test can resolve to Foo::<?>::test
	  // here so we cannot resolve this case
	  //
	  // canonical names:
	  //
	  // struct Foo<A>            -> Foo
	  // impl Foo<isize>::fn test -> Foo::isize::test
	  // impl Foo<f32>::fn test   -> Foo::f32::test
	  //
	  // Since there is the case we have the following paths for test:
	  //
	  // Foo::isize::test
	  // Foo::f32::test
	  // vs
	  // Foo::test
	  //
	  // but the lookup was simply Foo::test we must rely on type resolution
	  // to figure this type out in a similar fashion to method resolution
	  // with a probe phase

	  // nothing more we can do we need the type resolver to try and resolve
	  // this
	  return;
	}
    }

  // its fully resolved lets mark it as such
  if (resolved_node != UNKNOWN_NODEID)
    {
      if (segment_is_type)
	resolver->insert_resolved_type (expr_node_id, resolved_node);
      else
	resolver->insert_resolved_name (expr_node_id, resolved_node);

      resolver->insert_new_definition (expr_node_id,
				       Definition{expr_node_id, parent});
    }
}

// rust-ast-resolve-type.h

void
ResolveType::visit (AST::ArrayType &type)
{
  type.get_elem_type ()->accept_vis (*this);
  ResolveExpr::go (type.get_size_expr ().get (), type.get_node_id ());
}

// rust-ast-resolve-item.h

void
ResolveItem::resolve_impl_item (AST::TraitImplItem *item,
				const CanonicalPath &self)
{
  ResolveImplItems::go (item, self);
}

void
ResolveItem::resolve_impl_item (AST::InherentImplItem *item,
				const CanonicalPath &self)
{
  ResolveImplItems::go (item, self);
}

void
ResolveItem::resolve_extern_item (AST::ExternalItem *item)
{
  ResolveExternItem::go (item);
}

} // namespace Resolver
} // namespace Rust
