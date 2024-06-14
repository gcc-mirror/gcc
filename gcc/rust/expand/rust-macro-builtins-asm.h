
#include "rust-macro-builtins.h"
#include "rust-macro-builtins-helpers.h"
#include "expected.h"
#include "rust-macro-invoc-lexer.h"
#include "rust/ast/rust-expr.h"
namespace Rust {
// All the operands are called asm_args in rustc asm.rs, we create a struct that
// can store all of these AsmArgs This replaces the phase where we have to parse
// all operands.
class InlineAsmParseError
{
public:
  location_t locus;
  std::string message;
};
class InlineAsmContext
{
public:
  bool allow_templates;
  bool is_explicit;
  bool consumed_comma_without_formatted_string;
  AST::InlineAsm &inline_asm;
  Parser<MacroInvocLexer> &parser;
  int last_token_id;
  InlineAsmContext (AST::InlineAsm &inline_asm, Parser<MacroInvocLexer> &parser,
		    int last_token_id)
    : allow_templates (true), is_explicit (false),
      consumed_comma_without_formatted_string (false), inline_asm (inline_asm),
      parser (parser), last_token_id (last_token_id)
  {}

  bool is_global_asm () { return inline_asm.is_global_asm; }

  bool allows_templates () { return allow_templates; }

  void set_allow_templates (bool allow_templates)
  {
    this->allow_templates = allow_templates;
  }
};

// Expected calls
tl::expected<InlineAsmContext, std::string>
validate (InlineAsmContext inline_asm_ctx);

tl::expected<InlineAsmContext, std::string>
parse_asm_arg (InlineAsmContext inline_asm_ctx);

tl::expected<InlineAsmContext, std::string>
parse_format_strings (InlineAsmContext inline_asm_ctx);

tl::expected<InlineAsmContext, std::string>
parse_clobber_abi (InlineAsmContext inline_asm_ctx);

// From rustc
tl::expected<InlineAsmContext, std::string>
parse_reg_operand (InlineAsmContext inline_asm_ctx);

tl::optional<AST::Fragment>
parse_asm (location_t invoc_locus, AST::MacroInvocData &invoc,
	   bool is_global_asm, bool semicolon);

bool
check_identifier (Parser<MacroInvocLexer> &parser, std::string ident);

void
check_and_set (InlineAsmContext &inline_asm_ctx, AST::InlineAsmOption option);

// From rustc
int
parse_options (InlineAsmContext &inline_asm_ctx);

// From rustc
tl::optional<AST::InlineAsmRegOrRegClass>
parse_reg (InlineAsmContext &inline_asm_ctx);

tl::optional<std::string>
parse_format_string (InlineAsmContext &inline_asm_ctx);

tl::optional<std::string>
parse_label (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	     InlineAsmContext &inline_asm_ctx);

std::set<std::string> potentially_nonpromoted_keywords
  = {"in", "out", "lateout", "inout", "inlateout", "const", "sym", "label"};

} // namespace Rust