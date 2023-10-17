#include "rust-compile.h"
#include "libproc_macro_internal/proc_macro.h"
#include "rust-compile-context.h"
#include "rust-compile-base.h"

namespace Rust {
namespace Compile {

const std::string GCCRS_PROC_MACRO_SYMBOL_PREFIX = "__gccrs_proc_macro_";

// This namespace brings multiple function to build and initialize multiple
// structures that needs to get exposed in the final shared library for
// procedural macro crates.
//
// The compiler needs some additional metadata to find which function correspond
// to the desired macro. The library shall expose one entrypoint symbol leading
// to those metadata which in turn lead to the correct function.
// This namespace describes how to build and initialize those metadata
// structures. Those structure should be kept in sync with the structures in
// libproc_macro_internal/proc_macro.h describing how they should be read.
namespace {

// Namespace containing all functions to build the different types.
namespace build {

// Build an array of attribute type for derive procedural macros.
tree
attribute_array (std::vector<std::string> attributes)
{
  tree attribute_ptr = build_pointer_type (char_type_node);
  tree attribute_type = build_qualified_type (attribute_ptr, TYPE_QUAL_CONST);
  return build_array_type_nelts (attribute_type, attributes.size ());
}

// We're constructing the following structure:
//
// struct {
//     const char *trait_name;
//     const char **attributes;
//     std::uint64_t attr_size;
//     TokenStream (fndecl*) (TokenStream);
// }
// The resulting structure should be the same as `CustomDerive` in proc_macro.h
tree
derive_proc_macro ()
{
  tree char_ptr = build_pointer_type (char_type_node);
  tree const_char_type = build_qualified_type (char_ptr, TYPE_QUAL_CONST);
  auto name_field = Backend::typed_identifier ("trait_name", const_char_type,
					       BUILTINS_LOCATION);

  tree handle_ptr = build_pointer_type (void_type_node);
  auto fndecl_field
    = Backend::typed_identifier ("fndecl", handle_ptr, BUILTINS_LOCATION);

  tree attribute_ptr = build_pointer_type (const_ptr_type_node);
  auto attributes_field
    = Backend::typed_identifier ("attributes", attribute_ptr,
				 BUILTINS_LOCATION);

  auto size_field = Backend::typed_identifier ("attr_size", unsigned_type_node,
					       BUILTINS_LOCATION);

  return Backend::struct_type (
    {name_field, attributes_field, size_field, fndecl_field});
}

// We're constructing the following structure:
//
// struct {
//     const char *name;
//     TokenStream (fndecl*) (TokenStream);
// }
// The resulting structure should be the same as `Bang` in proc_macro.h
tree
bang_proc_macro ()
{
  tree char_ptr = build_pointer_type (char_type_node);
  tree const_char_type = build_qualified_type (char_ptr, TYPE_QUAL_CONST);
  Backend::typed_identifier name_field
    = Backend::typed_identifier ("name", const_char_type, BUILTINS_LOCATION);

  tree handle_ptr = ptr_type_node;
  Backend::typed_identifier fndecl_field
    = Backend::typed_identifier ("fndecl", handle_ptr, BUILTINS_LOCATION);

  return Backend::struct_type ({name_field, fndecl_field});
}

// Bang proc macros and attribute proc macros almost have the same members
// the function pointer type is not the same.
//
// We're constructing the following structure:
//
// struct {
//     const char *name;
//     TokenStream (fndecl*) (TokenStream, TokenStream);
// }
// The resulting structure should be the same as `Attribute` in proc_macro.h
tree
attribute_proc_macro ()
{
  return bang_proc_macro ();
}

// Build the union of all macro types. The resulting type should have the exact
// same representation as `ProcMacroPayload` in proc_macro.h
tree
proc_macro_payload ()
{
  tree bang = bang_proc_macro ();
  tree attribute = attribute_proc_macro ();
  tree derive = derive_proc_macro ();

  auto bang_field = Backend::typed_identifier ("bang", bang, BUILTINS_LOCATION);
  auto attribute_field
    = Backend::typed_identifier ("attribute", attribute, BUILTINS_LOCATION);
  auto derive_field
    = Backend::typed_identifier ("custom_derive", derive, BUILTINS_LOCATION);

  // We rely on the tag to represent the index of any union member. This means
  // we should keep those fields in the same order as the tag representation for
  // it to be kept in sync.
  // Hence why the following code exist: to keep in sync the field vector and
  // the tag enumeration.
  std::vector<Backend::typed_identifier> fields;
  fields.insert (fields.begin () + ProcMacro::CUSTOM_DERIVE, derive_field);
  fields.insert (fields.begin () + ProcMacro::ATTR, attribute_field);
  fields.insert (fields.begin () + ProcMacro::BANG, bang_field);

  return Backend::union_type (fields);
}

// Build the tagged union proc macro type. This type contains a payload as well
// as a tag to identify the contained member of the payload.
//
// struct {
//     unsigned short tag;
//     union { BangProcMacro , DeriveProcMacro, AttributeProcMacro} payload;
// }
tree
proc_macro ()
{
  auto union_field = proc_macro_payload ();
  auto payload_field
    = Backend::typed_identifier ("payload", union_field, BUILTINS_LOCATION);

  auto tag_field = Backend::typed_identifier ("tag", short_unsigned_type_node,
					      BUILTINS_LOCATION);

  return Backend::struct_type ({tag_field, payload_field});
}

// Build the `ProcmacroArray` structure
//
// struct {
//     std::uint64_t length;
//     Procmacro * macros;
// }
tree
proc_macro_buffer (tree proc_macro_type, size_t total_macro)
{
  auto length_field = Backend::typed_identifier ("length", unsigned_type_node,
						 BUILTINS_LOCATION);

  auto array_type = build_array_type_nelts (proc_macro_type, total_macro);
  auto macros_field
    = Backend::typed_identifier ("macros", array_type, BUILTINS_LOCATION);

  return Backend::struct_type ({length_field, macros_field});
}

// The entrypoint of a proc macro crate is a reference to the proc macro buffer
// `ProcmacroArray` defined in proc_macro.h
tree
entrypoint (tree proc_macro_buffer)
{
  return build_reference_type_for_mode (proc_macro_buffer, E_VOIDmode, false);
}

} // namespace build

// Functions to init all proc macro trees with the correct values from some
// macro information
namespace init {

// Initialize a derive proc macro structure
// - Store the trait name
// - Initialize the attribute array
// - Store the attribute array size
// - Store the address of the function
tree
derive_proc_macro (Context *ctx, CustomDeriveInfo infos)
{
  tree derive_proc_macro_type = build::derive_proc_macro ();
  tree trait_name = build_string_literal (infos.trait_name.c_str ());

  tree attribute_ptr;
  if (infos.attributes.size () == 0)
    {
      // Set a null pointer if there is no attributes
      attribute_ptr = HIRCompileBase::address_expression (null_pointer_node,
							  BUILTINS_LOCATION);
    }
  else
    {
      // Initialize the attribute array
      tree attribute_array_type = build::attribute_array (infos.attributes);

      std::vector<tree> attr_ctors;
      std::vector<unsigned long> indices;

      size_t index = 0;
      for (auto &attr : infos.attributes)
	{
	  attr_ctors.push_back (build_string_literal (attr.c_str ()));
	  indices.push_back (index);
	  index++;
	}

      tree attributes
	= Backend::array_constructor_expression (attribute_array_type, indices,
						 attr_ctors, BUILTINS_LOCATION);

      std::string attribute_var_name
	= GCCRS_PROC_MACRO_SYMBOL_PREFIX + infos.trait_name;
      Bvariable *attributes_var
	= Backend::global_variable (attribute_var_name.c_str (),
				    attribute_var_name.c_str (),
				    attribute_array_type, false /* internal */,
				    true /* hidden */, false /* no gc */,
				    BUILTINS_LOCATION);
      Backend::global_variable_set_init (attributes_var, attributes);
      ctx->push_var (attributes_var);

      attribute_ptr
	= HIRCompileBase::address_expression (attributes_var->get_decl (),
					      BUILTINS_LOCATION);
    }

  tree attr_size = build_int_cst (unsigned_type_node, infos.attributes.size ());

  tree handle
    = HIRCompileBase::address_expression (infos.fndecl, BUILTINS_LOCATION);

  return Backend::constructor_expression (derive_proc_macro_type, false,
					  {trait_name, attribute_ptr, attr_size,
					   handle},
					  -1 /* Structure: no index */,
					  BUILTINS_LOCATION);
}

// Initialize an attribute proc macro structure.
// - Store the name
// - Store the address of the function
tree
attribute_proc_macro (tree macro)
{
  tree attribute_proc_macro_type = build::attribute_proc_macro ();
  tree macro_name
    = build_string_literal (IDENTIFIER_POINTER (DECL_NAME (macro)));
  tree handle = HIRCompileBase::address_expression (macro, BUILTINS_LOCATION);

  return Backend::constructor_expression (attribute_proc_macro_type, false,
					  {macro_name, handle},
					  -1 /* Structure: No index */,
					  BUILTINS_LOCATION);
}

// Initialize a bang proc macro structure.
// - Store the name
// - Store the address of the function
tree
bang_proc_macro (tree macro)
{
  // Attribute and bang proc macros have the same structure, they can be
  // initialized with the same code.
  return attribute_proc_macro (macro);
}

// Initialize a proc macro structure from a given payload tree
tree
proc_macro (tree payload, tree proc_macro_type, ProcMacro::ProcmacroTag tag)
{
  auto discriminant = static_cast<int> (tag);

  tree macro_tag = build_int_cst (short_unsigned_type_node, discriminant);

  tree payload_union
    = Backend::constructor_expression (build::proc_macro_payload (), false,
				       {payload},
				       discriminant /* Union: member index */,
				       BUILTINS_LOCATION);

  return Backend::constructor_expression (proc_macro_type,
					  false /* invariant */,
					  {macro_tag, payload_union},
					  -1 /* Structure: No index */,
					  BUILTINS_LOCATION);
}

tree
proc_macro_array (Context *ctx, tree proc_macro_buffer_type,
		  tree proc_macro_type)
{
  std::vector<unsigned long> indexes;
  std::vector<tree> ctors;
  size_t index = 0;
  for (auto &macro : ctx->get_derive_proc_macros ())
    {
      tree derive = derive_proc_macro (ctx, macro);
      ctors.push_back (proc_macro (derive, proc_macro_type,
				   ProcMacro::ProcmacroTag::CUSTOM_DERIVE));
      indexes.push_back (index);
      index++;
    }
  for (auto &macro : ctx->get_attribute_proc_macros ())
    {
      tree attr = attribute_proc_macro (macro);

      ctors.push_back (
	proc_macro (attr, proc_macro_type, ProcMacro::ProcmacroTag::ATTR));
      indexes.push_back (index);
      index++;
    }
  for (auto &macro : ctx->get_bang_proc_macros ())
    {
      tree bang = bang_proc_macro (macro);

      ctors.push_back (
	proc_macro (bang, proc_macro_type, ProcMacro::ProcmacroTag::BANG));
      indexes.push_back (index);
      index++;
    }

  auto length = build_int_cst (unsigned_type_node, ctors.size ());
  auto array = Backend::array_constructor_expression (
    build_array_type_nelts (proc_macro_type, ctors.size ()), indexes, ctors,
    BUILTINS_LOCATION);
  return Backend::constructor_expression (proc_macro_buffer_type,
					  false /* invariant */,
					  {length, array},
					  -1 /* Structure: No index */,
					  BUILTINS_LOCATION);
}
} // namespace init

} // namespace

// Gather procedural macros and generate the metadata as well as the entrypoint
// for a procedural macro crate.
void
CompileCrate::add_proc_macro_symbols ()
{
  auto total_macros = ctx->get_attribute_proc_macros ().size ()
		      + ctx->get_bang_proc_macros ().size ()
		      + ctx->get_derive_proc_macros ().size ();

  tree pm_type = build::proc_macro ();
  tree pm_buffer_type = build::proc_macro_buffer (pm_type, total_macros);
  tree entrypoint_type = build::entrypoint (pm_buffer_type);

  std::string decl_symbol_name = generate_proc_macro_decls_symbol (
    0 /* FIXME: Change to stable crate id */);

  Bvariable *macro_decls
    = Backend::global_variable (decl_symbol_name.c_str (),
				decl_symbol_name.c_str (), entrypoint_type,
				false /* internal */, false /* not hidden */,
				false /* no gc */, BUILTINS_LOCATION);

  std::string buffer_name
    = GCCRS_PROC_MACRO_SYMBOL_PREFIX + "proc_macro_buffer";

  Bvariable *proc_macro_buffer
    = Backend::global_variable (buffer_name.c_str (), buffer_name.c_str (),
				pm_buffer_type, false /* internal */,
				true /* hidden */, false /* no gc */,
				BUILTINS_LOCATION);
  Backend::global_variable_set_init (
    proc_macro_buffer, init::proc_macro_array (ctx, pm_buffer_type, pm_type));
  ctx->push_var (proc_macro_buffer);

  Backend::global_variable_set_init (
    macro_decls,
    HIRCompileBase::address_expression (proc_macro_buffer->get_decl (),
					BUILTINS_LOCATION));

  ctx->push_var (macro_decls);
}

} // namespace Compile
} // namespace Rust
