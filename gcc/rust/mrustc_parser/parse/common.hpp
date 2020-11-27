/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * parse/common.hpp
 * - Common definitions used by the parser
 */
#ifndef PARSE_COMMON_HPP_INCLUDED
#define PARSE_COMMON_HPP_INCLUDED
#include <iostream>
#include "tokenstream.hpp"
#include "../ast/ast.hpp"

#define GET_TOK(tok, lex) ((tok = lex.getToken()).type())
#define PUTBACK(tok, lex) lex.putback( ::std::move(tok) )
#define LOOK_AHEAD(lex) (lex.lookahead(0))
#define GET_CHECK_TOK(tok, lex, exp) do {\
    if((tok = lex.getToken()).type() != exp) { \
        DEBUG("GET_CHECK_TOK " << __FILE__ << ":" << __LINE__); \
        throw ParseError::Unexpected(lex, tok, Token(exp));\
    }\
} while(0)
#define CHECK_TOK(tok, exp) do {\
    if(tok.type() != exp) { \
        DEBUG("CHECK_TOK " << __FILE__ << ":" << __LINE__); \
        throw ParseError::Unexpected(lex, tok, Token(exp));\
    } \
} while(0)

// --- path.cpp
enum eParsePathGenericMode
{
    PATH_GENERIC_NONE,
    PATH_GENERIC_EXPR,
    PATH_GENERIC_TYPE
};
extern AST::Path   Parse_Path(TokenStream& lex, eParsePathGenericMode generic_mode);    // Auto-determines
extern AST::Path   Parse_Path(TokenStream& lex, bool is_abs, eParsePathGenericMode generic_mode);
extern ::std::vector<AST::PathNode> Parse_PathNodes(TokenStream& lex, eParsePathGenericMode generic_mode);
extern AST::PathParams Parse_Path_GenericList(TokenStream& lex);


extern AST::HigherRankedBounds Parse_HRB(TokenStream& lex);
extern AST::AttributeList  Parse_ItemAttrs(TokenStream& lex);
extern void Parse_ParentAttrs(TokenStream& lex, AST::AttributeList& out);
extern AST::Attribute   Parse_MetaItem(TokenStream& lex);
extern ::AST::MacroInvocation Parse_MacroInvocation(ProtoSpan ps, ::std::string name, TokenStream& lex);
extern TypeRef     Parse_Type(TokenStream& lex, bool allow_trait_list = true);
extern AST::Pattern Parse_Pattern(TokenStream& lex, bool is_refutable);

extern void Parse_Impl_Item(TokenStream& lex, AST::Impl& impl);
extern void Parse_Mod_Item(TokenStream& lex, AST::Module& mod, AST::AttributeList meta_items);
extern ::AST::Named<::AST::Item> Parse_Mod_Item_S(TokenStream& lex, const AST::Module::FileInfo& mod_fileinfo, const ::AST::Path& mod_path, AST::AttributeList meta_items);
extern void Parse_ModRoot_Items(TokenStream& lex, AST::Module& mod);


extern AST::Expr   Parse_Expr(TokenStream& lex);
extern AST::Expr   Parse_ExprBlock(TokenStream& lex);
extern AST::ExprNodeP   Parse_Expr0(TokenStream& lex);
extern AST::ExprNodeP   Parse_ExprVal(TokenStream& lex);
extern AST::ExprNodeP Parse_ExprBlockNode(TokenStream& lex, bool is_unsafe=false);
extern AST::ExprNodeP Parse_ExprBlockLine(TokenStream& lex, bool *add_silence);
extern AST::ExprNodeP Parse_ExprBlockLine_WithItems(TokenStream& lex, ::std::shared_ptr<AST::Module>& local_mod, bool& add_silence_if_end);
extern AST::ExprNodeP Parse_Stmt(TokenStream& lex);

// unwrapped = Exclude the enclosing brackets (used by macro parse code)
extern TokenTree Parse_TT(TokenStream& lex, bool unwrapped);


extern bool Parse_IsTokValue(eTokenType tok_type);

#endif // PARSE_COMMON_HPP_INCLUDED
