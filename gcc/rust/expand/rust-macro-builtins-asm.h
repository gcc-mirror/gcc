
#include "rust-macro-builtins.h"
#include "rust-macro-builtins-helpers.h"
#include "rust-macro-invoc-lexer.h"
#include "rust/ast/rust-expr.h"
namespace Rust {
// All the operands are called asm_args in rustc asm.rs, we create a struct that
// can store all of these AsmArgs This replaces the phase where we have to parse
// all operands.
int
parseAsmArg (Parser<MacroInvocLexer> &p, TokenId last_token_id,
	     AST::InlineAsm &inlineAsm,
	     bool consumed_comma_without_formatted_string);
static tl::optional<AST::Fragment>
parse_global_asm (location_t invoc_locus, AST::MacroInvocData &invoc);
static tl::optional<AST::Fragment>
parse_nonglobal_asm (location_t invoc_locus, AST::MacroInvocData &invoc);
static tl::optional<AST::Fragment>
parse_asm (location_t invoc_locus, AST::MacroInvocData &invoc,
	   bool is_global_asm);

bool
check_identifier (Parser<MacroInvocLexer> &p, std::string ident);

void
check_and_set (Parser<MacroInvocLexer> &p, AST::InlineAsm &inlineAsm,
	       AST::InlineAsmOptions option);
// From rustc
int
parse_operand (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	       AST::InlineAsm &inlineAsm);

// From rustc
int
parse_options (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	       AST::InlineAsm &inlineAsm);

// From rustc
int
parse_reg (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	   AST::InlineAsm &inlineAsm, bool is_explicit);

int
parse_clobber_abi (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
		   AST::InlineAsm &inlineAsm);

} // namespace Rust