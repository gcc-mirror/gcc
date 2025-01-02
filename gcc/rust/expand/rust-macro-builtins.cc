// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#include "expected.h"
#include "libproc_macro_internal/tokenstream.h"
#include "optional.h"
#include "rust-ast-full-decls.h"
#include "rust-builtin-ast-nodes.h"
#include "rust-expand-format-args.h"
#include "rust-token-converter.h"
#include "rust-system.h"
#include "rust-macro-builtins.h"
#include "rust-ast-fragment.h"
#include "rust-ast.h"
#include "rust-cfg-strip.h"
#include "rust-diagnostics.h"
#include "rust-early-name-resolver.h"
#include "rust-expr.h"
#include "rust-lex.h"
#include "rust-macro-invoc-lexer.h"
#include "rust-macro.h"
#include "rust-parse.h"
#include "rust-session-manager.h"
#include "rust-attribute-values.h"
#include "rust-fmt.h"
#include "rust-token.h"

#include "rust-macro-builtins-helpers.h"
namespace Rust {

const BiMap<std::string, BuiltinMacro> MacroBuiltin::builtins = {{
  {"assert", BuiltinMacro::Assert},
  {"file", BuiltinMacro::File},
  {"line", BuiltinMacro::Line},
  {"column", BuiltinMacro::Column},
  {"include_bytes", BuiltinMacro::IncludeBytes},
  {"include_str", BuiltinMacro::IncludeStr},
  {"stringify", BuiltinMacro::Stringify},
  {"compile_error", BuiltinMacro::CompileError},
  {"concat", BuiltinMacro::Concat},
  {"env", BuiltinMacro::Env},
  {"option_env", BuiltinMacro::OptionEnv},
  {"cfg", BuiltinMacro::Cfg},
  {"include", BuiltinMacro::Include},
  {"format_args", BuiltinMacro::FormatArgs},
  {"format_args_nl", BuiltinMacro::FormatArgsNl},
  {"concat_idents", BuiltinMacro::ConcatIdents},
  {"module_path", BuiltinMacro::ModulePath},
  {"asm", BuiltinMacro::Asm},
  {"llvm_asm", BuiltinMacro::LlvmAsm},
  {"global_asm", BuiltinMacro::GlobalAsm},
  {"log_syntax", BuiltinMacro::LogSyntax},
  {"trace_macros", BuiltinMacro::TraceMacros},
  {"test", BuiltinMacro::Test},
  {"bench", BuiltinMacro::Bench},
  {"test_case", BuiltinMacro::TestCase},
  {"global_allocator", BuiltinMacro::GlobalAllocator},
  {"cfg_accessible", BuiltinMacro::CfgAccessible},
  {"RustcEncodable", BuiltinMacro::RustcDecodable},
  {"RustcDecodable", BuiltinMacro::RustcEncodable},
  {"Clone", BuiltinMacro::Clone},
  {"Copy", BuiltinMacro::Copy},
  {"Debug", BuiltinMacro::Debug},
  {"Default", BuiltinMacro::Default},
  {"Eq", BuiltinMacro::Eq},
  {"PartialEq", BuiltinMacro::PartialEq},
  {"Ord", BuiltinMacro::Ord},
  {"PartialOrd", BuiltinMacro::PartialOrd},
  {"Hash", BuiltinMacro::Hash},

}};

AST::MacroTranscriberFunc
format_args_maker (AST::FormatArgs::Newline nl)
{
  return [nl] (location_t loc, AST::MacroInvocData &invoc) {
    return MacroBuiltin::format_args_handler (loc, invoc, nl);
  };
}

std::unordered_map<std::string, AST::MacroTranscriberFunc>
  MacroBuiltin::builtin_transcribers = {
    {"assert", MacroBuiltin::assert_handler},
    {"file", MacroBuiltin::file_handler},
    {"line", MacroBuiltin::line_handler},
    {"column", MacroBuiltin::column_handler},
    {"include_bytes", MacroBuiltin::include_bytes_handler},
    {"include_str", MacroBuiltin::include_str_handler},
    {"stringify", MacroBuiltin::stringify_handler},
    {"compile_error", MacroBuiltin::compile_error_handler},
    {"concat", MacroBuiltin::concat_handler},
    {"env", MacroBuiltin::env_handler},
    {"cfg", MacroBuiltin::cfg_handler},
    {"include", MacroBuiltin::include_handler},
    {"format_args", format_args_maker (AST::FormatArgs::Newline::No)},
    {"format_args_nl", format_args_maker (AST::FormatArgs::Newline::Yes)},
    /* Unimplemented macro builtins */
    {"option_env", MacroBuiltin::sorry},
    {"concat_idents", MacroBuiltin::sorry},
    {"module_path", MacroBuiltin::sorry},
    {"asm", MacroBuiltin::sorry},
    {"llvm_asm", MacroBuiltin::sorry},
    {"global_asm", MacroBuiltin::sorry},
    {"log_syntax", MacroBuiltin::sorry},
    {"trace_macros", MacroBuiltin::sorry},
    {"test", MacroBuiltin::sorry},
    {"bench", MacroBuiltin::sorry},
    {"test_case", MacroBuiltin::sorry},
    {"global_allocator", MacroBuiltin::sorry},
    {"cfg_accessible", MacroBuiltin::sorry},
    /* Derive builtins do not need a real transcriber, but still need one. It
       should however never be called since builtin derive macros get expanded
       differently, and benefit from knowing on what kind of items they are
       applied (struct, enums, unions) rather than receiving a list of tokens
       like regular builtin macros */
    {"RustcEncodable", MacroBuiltin::proc_macro_builtin},
    {"RustcDecodable", MacroBuiltin::proc_macro_builtin},
    {"Clone", MacroBuiltin::proc_macro_builtin},
    {"Copy", MacroBuiltin::proc_macro_builtin},
    {"Debug", MacroBuiltin::proc_macro_builtin},
    {"Default", MacroBuiltin::proc_macro_builtin},
    {"Eq", MacroBuiltin::proc_macro_builtin},
    {"PartialEq", MacroBuiltin::proc_macro_builtin},
    {"Ord", MacroBuiltin::proc_macro_builtin},
    {"PartialOrd", MacroBuiltin::proc_macro_builtin},
    {"Hash", MacroBuiltin::proc_macro_builtin},
};

tl::optional<BuiltinMacro>
builtin_macro_from_string (const std::string &identifier)
{
  auto macro = MacroBuiltin::builtins.lookup (identifier);
  rust_assert (macro.has_value ());

  return macro;
}

tl::optional<AST::Fragment>
MacroBuiltin::sorry (location_t invoc_locus, AST::MacroInvocData &invoc)
{
  rust_sorry_at (invoc_locus, "unimplemented builtin macro: %qs",
		 invoc.get_path ().as_string ().c_str ());

  return AST::Fragment::create_error ();
}

tl::optional<AST::Fragment>
MacroBuiltin::proc_macro_builtin (location_t invoc_locus,
				  AST::MacroInvocData &invoc)
{
  rust_error_at (invoc_locus, "cannot invoke derive macro: %qs",
		 invoc.get_path ().as_string ().c_str ());

  return AST::Fragment::create_error ();
}

} // namespace Rust
