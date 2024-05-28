
#include "rust-macro-builtins.h"
#include "rust-macro-builtins-helpers.h"
#include "rust-macro-invoc-lexer.h"
#include "rust/ast/rust-expr.h"
namespace Rust {
// All the operands are called asm_args in rustc asm.rs, we create a struct that
// can store all of these AsmArgs This replaces the phase where we have to parse
// all operands.

class InlineAsmContext
{
public:
  bool allow_templates;
  bool is_explicit;
  bool consumed_comma_without_formatted_string;
  AST::InlineAsm &inlineAsm;
  InlineAsmContext (AST::InlineAsm &inlineAsm)
    : allow_templates (true), is_explicit (false),
      consumed_comma_without_formatted_string (false), inlineAsm (inlineAsm)
  {}

  bool is_global_asm () { return inlineAsm.is_global_asm; }

  bool allows_templates () { return allow_templates; }

  void set_allow_templates (bool allow_templates)
  {
    this->allow_templates = allow_templates;
  }
};

int
parse_asm_arg (Parser<MacroInvocLexer> &p, TokenId last_token_id,
	       InlineAsmContext &inlineAsmCtx);

tl::optional<AST::Fragment>
parse_asm (location_t invoc_locus, AST::MacroInvocData &invoc,
	   bool is_global_asm);

bool
check_identifier (Parser<MacroInvocLexer> &p, std::string ident);

void
check_and_set (Parser<MacroInvocLexer> &p, InlineAsmContext &inlineAsmCtx,
	       AST::InlineAsmOption option);
// From rustc
int
parse_operand (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	       InlineAsmContext &inlineAsmCtx);

// From rustc
tl::optional<AST::InlineAsmOperand>
parse_reg_operand (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
		   InlineAsmContext &inlineAsmCtx);

// From rustc
int
parse_options (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	       InlineAsmContext &inlineAsmCtx);

// From rustc
tl::optional<AST::InlineAsmRegOrRegClass>
parse_reg (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	   InlineAsmContext &inlineAsmCtx);

int
parse_clobber_abi (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
		   InlineAsmContext &inlineAsmCtx);

} // namespace Rust