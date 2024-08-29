
#include "rust-ast-fragment.h"
#include "rust-macro-builtins.h"
#include "rust-macro-builtins-helpers.h"
#include "expected.h"
#include "rust-macro-invoc-lexer.h"
#include "rust/ast/rust-expr.h"
#include "system.h"
namespace Rust {

enum InlineAsmParseError
{
  // Enum for InlineAsmParseError

  // Currently with two error, COMMITTED AND NONCOMMITTED (to a token),
  // which directs the parser to either bubbles the error up, or keep on going
  // (vertical or horizontal)

  // COMMITTED can be use as a way for parser to bubble up
  // after it has exhausted its search space despite it not having committed to
  // any token

  COMMITTED,
  NONCOMMITED,
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

  InlineAsmContext (const InlineAsmContext &inline_asm_ctx)
    : allow_templates (inline_asm_ctx.allow_templates),
      is_explicit (inline_asm_ctx.is_explicit),
      consumed_comma_without_formatted_string (false),
      inline_asm (inline_asm_ctx.inline_asm), parser (inline_asm_ctx.parser),
      last_token_id (inline_asm_ctx.last_token_id)
  {}
  // explicit InlineAsmContext (InlineAsmContext&& inline_asm_ctx)
  //   : allow_templates (inline_asm_ctx.allow_templates), is_explicit
  //   (inline_asm_ctx.is_explicit),
  //     consumed_comma_without_formatted_string (false), inline_asm
  //     (inline_asm_ctx.inline_asm), parser (inline_asm_ctx.parser),
  //     last_token_id (inline_asm_ctx.last_token_id)
  // {}

  // InlineAsmContext(tl::expected<InlineAsmContext, InlineAsmParseError>
  // &expected)
  //     : allow_templates(expected->allow_templates),
  //     is_explicit(expected->is_explicit),
  //       consumed_comma_without_formatted_string(expected->consumed_comma_without_formatted_string),
  //       inline_asm(expected->inline_asm), parser(expected->parser),
  //       last_token_id(expected->last_token_id)
  // {

  // }
  InlineAsmContext &operator= (const InlineAsmContext &inline_asm_ctx)
  {
    allow_templates = inline_asm_ctx.allow_templates;
    is_explicit = inline_asm_ctx.is_explicit;
    consumed_comma_without_formatted_string = false;
    last_token_id = inline_asm_ctx.last_token_id;
    return *this;
  }

  bool is_global_asm () { return inline_asm.is_global_asm; }

  bool allows_templates () { return allow_templates; }

  void set_allow_templates (bool allow_templates)
  {
    this->allow_templates = allow_templates;
  }
};
WARN_UNUSED_RESULT
tl::expected<InlineAsmContext, InlineAsmParseError>
expand_inline_asm_strings (InlineAsmContext inline_asm_ctx);

// Expected calls
WARN_UNUSED_RESULT
tl::expected<InlineAsmContext, InlineAsmParseError>
validate (InlineAsmContext inline_asm_ctx);

WARN_UNUSED_RESULT
tl::expected<InlineAsmContext, InlineAsmParseError>
parse_asm_arg (InlineAsmContext inline_asm_ctx);

WARN_UNUSED_RESULT
tl::expected<InlineAsmContext, InlineAsmParseError>
parse_format_strings (InlineAsmContext inline_asm_ctx);

WARN_UNUSED_RESULT
tl::expected<InlineAsmContext, InlineAsmParseError>
parse_clobber_abi (InlineAsmContext inline_asm_ctx);

// From rustc
WARN_UNUSED_RESULT
tl::expected<InlineAsmContext, InlineAsmParseError>
parse_reg_operand (InlineAsmContext inline_asm_ctx);

WARN_UNUSED_RESULT
tl::expected<InlineAsmContext, InlineAsmParseError>
parse_reg_operand_in (InlineAsmContext inline_asm_ctx);

WARN_UNUSED_RESULT
tl::expected<InlineAsmContext, InlineAsmParseError>
parse_reg_operand_out (InlineAsmContext inline_asm_ctx);

WARN_UNUSED_RESULT
tl::expected<InlineAsmContext, InlineAsmParseError>
parse_reg_operand_lateout (InlineAsmContext inline_asm_ctx);

WARN_UNUSED_RESULT
tl::expected<InlineAsmContext, InlineAsmParseError>
parse_reg_operand_inout (InlineAsmContext inline_asm_ctx);

WARN_UNUSED_RESULT
tl::expected<InlineAsmContext, InlineAsmParseError>
parse_reg_operand_inlateout (InlineAsmContext inline_asm_ctx);

WARN_UNUSED_RESULT
tl::expected<InlineAsmContext, InlineAsmParseError>
parse_reg_operand_const (InlineAsmContext inline_asm_ctx);

WARN_UNUSED_RESULT
tl::expected<InlineAsmContext, InlineAsmParseError>
parse_reg_operand_sym (InlineAsmContext inline_asm_ctx);

WARN_UNUSED_RESULT
tl::expected<InlineAsmContext, InlineAsmParseError>
parse_reg_operand_unexpected (InlineAsmContext inline_asm_ctx);

WARN_UNUSED_RESULT
tl::optional<AST::Fragment>
parse_asm (location_t invoc_locus, AST::MacroInvocData &invoc,
	   AST::InvocKind semicolon, AST::AsmKind is_global_asm);

WARN_UNUSED_RESULT
bool
check_identifier (Parser<MacroInvocLexer> &parser, std::string ident);

void
check_and_set (InlineAsmContext &inline_asm_ctx, AST::InlineAsmOption option);

// From rustc
WARN_UNUSED_RESULT
tl::expected<InlineAsmContext, InlineAsmParseError>
parse_options (InlineAsmContext &inline_asm_ctx);

// From rustc
WARN_UNUSED_RESULT
tl::optional<AST::InlineAsmRegOrRegClass>
parse_reg (InlineAsmContext &inline_asm_ctx);

WARN_UNUSED_RESULT
tl::optional<std::string>
parse_format_string (InlineAsmContext &inline_asm_ctx);

WARN_UNUSED_RESULT
tl::optional<std::string>
parse_label (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	     InlineAsmContext &inline_asm_ctx);

} // namespace Rust
