
#include "rust-macro-builtins.h"
#include "rust-macro-builtins-helpers.h"
#include "rust-macro-invoc-lexer.h"

namespace Rust {
struct AsmParseError
{
};

// This is just an enum to hold some operands right now.
enum InlineAsmDirSpec
{
  In,
  Out,
  InOut,
  SplitInOut,
  InLateOut, // TODO: This is not present in rust's asm.rs
  Const,     // TODO: This is not present in ABNF
  Sym,	     // TODO: This is not present in ABNF
  Label,     // TODO: This is not present in ABNF
};

// Place holder for classes
enum InlineAsmRegOrRegClass
{
  InlineAsmReg,
  Reg,
};

typedef std::string symbol_name;
typedef std::vector<AST::Expr> Templates;
typedef std::vector<InlineAsmDirSpec> Operands;
typedef std::map<std::string, int> RegisterArgs;
typedef std::vector<symbol_name> ClobberAbis;
typedef std::map<symbol_name, int> NamedValues;
typedef std::set<std::string> InlineAsmOptions;

struct AsmArg
{
  Templates templates;
  Operands operands;
  std::map<symbol_name, int> named_values;
  RegisterArgs register_arguments;
  ClobberAbis clobber_abis;
  InlineAsmOptions options;
  std::vector<InlineAsmOptions>
    options_span; // TODO: @badumbatish @jjasmine I have no idea what span do, i
		  // copied it out of rustc_builtin_macros/src/asm.rs
};

// All the operands are called asm_args in rustc asm.rs, we create a struct that
// can store all of these AsmArgs This replaces the phase where we have to parse
// all operands.
tl::optional<AsmArg>
parseAsmArg (Parser<MacroInvocLexer> &p, TokenId last_token_id,
	     bool is_global_asm);
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
check_and_set (Parser<MacroInvocLexer> &p, AsmArg &args, std::string option);
// From rustc
int
parse_operand (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	       AsmArg &args);

// From rustc
int
parse_options (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
	       AsmArg &args, bool is_global_asm);

// From rustc
tl::optional<InlineAsmRegOrRegClass>
parse_reg (Parser<MacroInvocLexer> &parser, TokenId last_token_id, AsmArg &args,
	   bool is_explicit);

int
parse_clobber_abi (Parser<MacroInvocLexer> &parser, TokenId last_token_id,
		   AsmArg &args);

} // namespace Rust