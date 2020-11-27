#include "rust-system.h"
#include "node.h"


static int const indent_step = 4;

void print_indent(int depth);

#define PUSHBACK_LEN 4	

static char pushback[PUSHBACK_LEN];	
static int verbose;
struct node* nodes = NULL;
int n_nodes;

void
parser_init(int v)
{
    verbose = v;
    n_nodes = 0;
    memset(pushback, '\0', PUSHBACK_LEN);
}

// Note: this does nothing if the pushback queue is full. As long as	
// there aren't more than PUSHBACK_LEN consecutive calls to push_back	
// in an action, this shouldn't be a problem.	
void push_back(char c)
{	
    for (int i = 0; i < PUSHBACK_LEN; ++i) {	
        if (pushback[i] == '\0') {	
            pushback[i] = c;	
            break;	
        }	
    }	
}

Node
mk_node(ast_opcode_t op, int n, ...)
{
    unsigned sz = sizeof(struct node);
    struct node* nd = (struct node *)xmalloc(sz);
    nd->elems = (struct node **)xcalloc(n, sizeof(struct node*));

    nd->prev = NULL;	
    nd->next = nodes;	
    if (nodes) {
        nodes->prev = nd;
    }	
    nodes = nd;	

    nd->op = op;
    nd->value = NULL;
    nd->n_elems = n;
    
    va_list ap;	
    va_start(ap, n);
    for (int i = 0; i < n; ++i)
    {
        struct node *nn = va_arg(ap, struct node *);	
        nd->elems[i] = nn;
    }
    va_end(ap);
    
    n_nodes++;	
    return nd;	
}

Node
mk_node_value(ast_opcode_t op, char* value, int n, ...)
{
    unsigned sz = sizeof(struct node);
    struct node* nd = (struct node *)xmalloc(sz);
    nd->elems = (struct node **)xcalloc(n, sizeof(struct node*));

    nd->prev = NULL;	
    nd->next = nodes;	
    if (nodes) {
        nodes->prev = nd;
    }	
    nodes = nd;	

    nd->op = op;
    nd->value = value;
    nd->n_elems = n;
    
    va_list ap;	
    va_start(ap, n);
    for (int i = 0; i < n; ++i)
    {
        struct node *nn = va_arg(ap, struct node *);	
        nd->elems[i] = nn;
    }
    va_end(ap);
    
    n_nodes++;	
    return nd;	
}	

Node
mk_atom(ast_opcode_t op, const char *value)
{
    return value == NULL ?
        mk_empty_atom(op) :
        mk_node_value(op, xstrdup(value), 0);
}

Node
mk_empty_atom(ast_opcode_t op)
{
    return mk_node_value(op, NULL, 0);
}

Node
mk_none()
{
    return mk_atom(NN_NONE, NULL);
}	

Node
ext_node(Node nd, int n, ...)
{
    int c = nd->n_elems + n;	
    unsigned sz = sizeof(struct node) + (c * sizeof(struct node *));	

    if (nd->next) {	
        nd->next->prev = nd->prev;	
    }	
    if (nd->prev) {	
        nd->prev->next = nd->next;	
    }	
    nd = (struct node*)xrealloc(nd, sz);
    nd->prev = NULL;	
    nd->next = nodes;	
    nodes->prev = nd;	
    nodes = nd;

    va_list ap;
    va_start(ap, n);
    for (int i = 0; i < n; ++i) {
        struct node* nn = va_arg(ap, struct node *);
        nd->elems[nd->n_elems++] = nn;	
    }
    va_end(ap);
    
    return nd;	
}	


void
print_indent(int depth) {
    while (depth) {	
        if (depth-- % indent_step == 0) {	
            printf("|");	
        } else {	
            printf(" ");	
        }	
    }	
}	

void
print_node(Node n, int depth)
{
    print_indent(depth);
    
    if (n->n_elems == 0) {
        printf("%s -> %s\n", NODE_TYPE_STR(n), NODE_TYPE_VALUE_STR(n));
        return;
    }
    
    printf("(%s\n", NODE_TYPE_STR(n));	
    for (int i = 0; i < n->n_elems; ++i) {
        print_node(n->elems[i], depth + indent_step);
    }
    print_indent(depth);
    printf(")\n");
}

const char*
get_ast_op_string(ast_opcode_t op)
{
    switch (op) {
    case NN_ViewItemExternFn:
	return "NN_ViewItemExternFn";
    case NN_TyParam:
	return "NN_TyParam";
    case NN_DeclLocal:
	return "NN_DeclLocal";
    case NN_DocComment:
	return "NN_DocComment";
    case NN_WherePredicate:
	return "NN_WherePredicate";
    case NN_ExprAddrOf:
	return "NN_ExprAddrOf";
    case NN_TypeMethod:
	return "NN_TypeMethod";
    case NN_crate:
	return "NN_crate";
    case NN_DefaultFieldInit:
	return "NN_DefaultFieldInit";
    case NN_ExprBinary:
	return "NN_ExprBinary";
    case NN_ExprAssignBitAnd:
	return "NN_ExprAssignBitAnd";
    case NN_TyFnDecl:
	return "NN_TyFnDecl";
    case NN_Public:
	return "NN_Public";
    case NN_Pats:
	return "NN_Pats";
    case NN_ViewPathSimple:
	return "NN_ViewPathSimple";
    case NN_TyParams:
	return "NN_TyParams";
    case NN_ExprIf:
	return "NN_ExprIf";
    case NN_Args:
	return "NN_Args";
    case NN_MetaItems:
	return "NN_MetaItems";
    case NN_AttrsAndVis:
	return "NN_AttrsAndVis";
    case NN_LitInteger:
	return "NN_LitInteger";
    case NN_ExprPath:
	return "NN_ExprPath";
    case NN_ExprField:
	return "NN_ExprField";
    case NN_TyDefault:
	return "NN_TyDefault";
    case NN_ExprAssignAdd:
	return "NN_ExprAssignAdd";
    case NN_ExprYield:
	return "NN_ExprYield";
    case NN_ImplItems:
	return "NN_ImplItems";
    case NN_TyRptr:
	return "NN_TyRptr";
    case NN_TySum:
	return "NN_TySum";
    case NN_ExprAssignDiv:
	return "NN_ExprAssignDiv";
    case NN_PatTup:
	return "NN_PatTup";
    case NN_ViewPath:
	return "NN_ViewPath";
    case NN_ItemImplNeg:
	return "NN_ItemImplNeg";
    case NN_ExprWhile:
	return "NN_ExprWhile";
    case NN_WherePredicates:
	return "NN_WherePredicates";
    case NN_FieldInits:
	return "NN_FieldInits";
    case NN_PatQualifiedPath:
	return "NN_PatQualifiedPath";
    case NN_ExprMatch:
	return "NN_ExprMatch";
    case NN_Binding:
	return "NN_Binding";
    case NN_ExprAssignBitOr:
	return "NN_ExprAssignBitOr";
    case NN_ItemImpl:
	return "NN_ItemImpl";
    case NN_ForeignItem:
	return "NN_ForeignItem";
    case NN_Arg:
	return "NN_Arg";
    case NN_Bindings:
	return "NN_Bindings";
    case NN_PatWild:
	return "NN_PatWild";
    case NN_ExprTypeAscr:
	return "NN_ExprTypeAscr";
    case NN_ExprBreak:
	return "NN_ExprBreak";
    case NN_ExprIfLet:
	return "NN_ExprIfLet";
    case NN_Inherited:
	return "NN_Inherited";
    case NN_TyPtr:
	return "NN_TyPtr";
    case NN_Arms:
	return "NN_Arms";
    case NN_LitByteStr:
	return "NN_LitByteStr";
    case NN_ItemConst:
	return "NN_ItemConst";
    case NN_ItemImplDefault:
	return "NN_ItemImplDefault";
    case NN_VecRepeat:
	return "NN_VecRepeat";
    case NN_TTTok:
	return "NN_TTTok";
    case NN_RetTy:
	return "NN_RetTy";
    case NN_TySumsAndBindings:
	return "NN_TySumsAndBindings";
    case NN_MetaList:
	return "NN_MetaList";
    case NN_ExprQualifiedPath:
	return "NN_ExprQualifiedPath";
    case NN_ExprAssignBitXor:
	return "NN_ExprAssignBitXor";
    case NN_Macro:
	return "NN_Macro";
    case NN_Super:
	return "NN_Super";
    case NN_ForeignFn:
	return "NN_ForeignFn";
    case NN_SelfLower:
	return "NN_SelfLower";
    case NN_ExprAssign:
	return "NN_ExprAssign";
    case NN_OuterAttrs:
	return "NN_OuterAttrs";
    case NN_ItemMacro:
	return "NN_ItemMacro";
    case NN_ForeignItems:
	return "NN_ForeignItems";
    case NN_Items:
	return "NN_Items";
    case NN_TraitMacroItem:
	return "NN_TraitMacroItem";
    case NN_ExprWhileLet:
	return "NN_ExprWhileLet";
    case NN_BindByRef:
	return "NN_BindByRef";
    case NN_ImplType:
	return "NN_ImplType";
    case NN_ExprLoop:
	return "NN_ExprLoop";
    case NN_SelfRegion:
	return "NN_SelfRegion";
    case NN_TyTup:
	return "NN_TyTup";
    case NN_PatUnit:
	return "NN_PatUnit";
    case NN_ForInType:
	return "NN_ForInType";
    case NN_FnDecl:
	return "NN_FnDecl";
    case NN_PatUniq:
	return "NN_PatUniq";
    case NN_UnsafeBlock:
	return "NN_UnsafeBlock";
    case NN_TyQualifiedPath:
	return "NN_TyQualifiedPath";
    case NN_TyBox:
	return "NN_TyBox";
    case NN_PatEnum:
	return "NN_PatEnum";
    case NN_TyClosure:
	return "NN_TyClosure";
    case NN_ForSized:
	return "NN_ForSized";
    case NN_ExprAssignSub:
	return "NN_ExprAssignSub";
    case NN_GenericValues:
	return "NN_GenericValues";
    case NN_TyPath:
	return "NN_TyPath";
    case NN_PatIdent:
	return "NN_PatIdent";
    case NN_ConstDefault:
	return "NN_ConstDefault";
    case NN_WhereClause:
	return "NN_WhereClause";
    case NN_MutImmutable:
	return "NN_MutImmutable";
    case NN_TyMacro:
	return "NN_TyMacro";
    case NN_ExprTupleIndex:
	return "NN_ExprTupleIndex";
    case NN_ImplMacroItem:
	return "NN_ImplMacroItem";
    case NN_ExprTry:
	return "NN_ExprTry";
    case NN_ExprCast:
	return "NN_ExprCast";
    case NN_Provided:
	return "NN_Provided";
    case NN_PatRegion:
	return "NN_PatRegion";
    case NN_ExprAssignShl:
	return "NN_ExprAssignShl";
    case NN_static_lifetime:
	return "NN_static_lifetime";
    case NN_MetaNameValue:
	return "NN_MetaNameValue";
    case NN_ExprStruct:
	return "NN_ExprStruct";
    case NN_ExprAssignShr:
	return "NN_ExprAssignShr";
    case NN_FieldInit:
	return "NN_FieldInit";
    case NN_Generics:
	return "NN_Generics";
    case NN_ExprForLoop:
	return "NN_ExprForLoop";
    case NN_Method:
	return "NN_Method";
    case NN_TySums:
	return "NN_TySums";
    case NN_ExprVec:
	return "NN_ExprVec";
    case NN_LitChar:
	return "NN_LitChar";
    case NN_PatFields:
	return "NN_PatFields";
    case NN_PatMac:
	return "NN_PatMac";
    case NN_InnerAttr:
	return "NN_InnerAttr";
    case NN_ViewPathGlob:
	return "NN_ViewPathGlob";
    case NN_ItemStatic:
	return "NN_ItemStatic";
    case NN_PatRange:
	return "NN_PatRange";
    case NN_LitBool:
	return "NN_LitBool";
    case NN_IdentsOrSelf:
	return "NN_IdentsOrSelf";
    case NN_ItemFn:
	return "NN_ItemFn";
    case NN_ExprCall:
	return "NN_ExprCall";
    case NN_Default:
	return "NN_Default";
    case NN_trait:
	return "NN_trait";
    case NN_Required:
	return "NN_Required";
    case NN_TyInfer:
	return "NN_TyInfer";
    case NN_ConstTraitItem:
	return "NN_ConstTraitItem";
    case NN_SelfStatic:
	return "NN_SelfStatic";
    case NN_ExprAssignRem:
	return "NN_ExprAssignRem";
    case NN_PatLit:
	return "NN_PatLit";
    case NN_PatField:
	return "NN_PatField";
    case NN_TyNil:
	return "NN_TyNil";
    case NN_EnumDef:
	return "NN_EnumDef";
    case NN_TypeTraitItem:
	return "NN_TypeTraitItem";
    case NN_PatVec:
	return "NN_PatVec";
    case NN_ident:
	return "NN_ident";
    case NN_MetaWord:
	return "NN_MetaWord";
    case NN_Lifetimes:
	return "NN_Lifetimes";
    case NN_LitStr:
	return "NN_LitStr";
    case NN_SelfPath:
	return "NN_SelfPath";
    case NN_bounds:
	return "NN_bounds";
    case NN_EnumDefs:
	return "NN_EnumDefs";
    case NN_ltbounds:
	return "NN_ltbounds";
    case NN_ViewItemUse:
	return "NN_ViewItemUse";
    case NN_ItemImplDefaultNeg:
	return "NN_ItemImplDefaultNeg";
    case NN_PatVecElts:
	return "NN_PatVecElts";
    case NN_TokenTrees:
	return "NN_TokenTrees";
    case NN_semicolon:
	return "NN_semicolon";
    case NN_ItemMod:
	return "NN_ItemMod";
    case NN_ExprUnary:
	return "NN_ExprUnary";
    case NN_ExprBlock:
	return "NN_ExprBlock";
    case NN_ItemUnsafeFn:
	return "NN_ItemUnsafeFn";
    case NN_EnumArgs:
	return "NN_EnumArgs";
    case NN_ViewItemExternCrate:
	return "NN_ViewItemExternCrate";
    case NN_lifetime:
	return "NN_lifetime";
    case NN_StructFields:
	return "NN_StructFields";
    case NN_ExprRet:
	return "NN_ExprRet";
    case NN_ExprAssignMul:
	return "NN_ExprAssignMul";
    case NN_ItemStruct:
	return "NN_ItemStruct";
    case NN_LitFloat:
	return "NN_LitFloat";
    case NN_TyTypeof:
	return "NN_TyTypeof";
    case NN_exprs:
	return "NN_exprs";
    case NN_InnerAttrs:
	return "NN_InnerAttrs";
    case NN_PolyBound:
	return "NN_PolyBound";
    case NN_TyVec:
	return "NN_TyVec";
    case NN_ArmNonblock:
	return "NN_ArmNonblock";
    case NN_ViewPathList:
	return "NN_ViewPathList";
    case NN_TTDelim:
	return "NN_TTDelim";
    case NN_ItemForeignMod:
	return "NN_ItemForeignMod";
    case NN_PatTupElts:
	return "NN_PatTupElts";
    case NN_ExprMac:
	return "NN_ExprMac";
    case NN_ExprParen:
	return "NN_ExprParen";
    case NN_StaticItem:
	return "NN_StaticItem";
    case NN_ItemTrait:
	return "NN_ItemTrait";
    case NN_ExprRange:
	return "NN_ExprRange";
    case NN_ItemUnion:
	return "NN_ItemUnion";
    case NN_ExprIndex:
	return "NN_ExprIndex";
    case NN_ArmBlock:
	return "NN_ArmBlock";
    case NN_DefaultUnsafe:
	return "NN_DefaultUnsafe";
    case NN_PatStruct:
	return "NN_PatStruct";
    case NN_MutMutable:
	return "NN_MutMutable";
    case NN_ExprLit:
	return "NN_ExprLit";
    case NN_TyFixedLengthVec:
	return "NN_TyFixedLengthVec";
    case NN_ImplConst:
	return "NN_ImplConst";
    case NN_InferrableParams:
	return "NN_InferrableParams";
    case NN_Item:
	return "NN_Item";
    case NN_TraitItems:
	return "NN_TraitItems";
    case NN_MacroExpr:
	return "NN_MacroExpr";
    case NN_LitByte:
	return "NN_LitByte";
    case NN_InferrableParam:
	return "NN_InferrableParam";
    case NN_BindByValue:
	return "NN_BindByValue";
    case NN_StructField:
	return "NN_StructField";
    case NN_Unsafe:
	return "NN_Unsafe";
    case NN_ViewPathListEmpty:
	return "NN_ViewPathListEmpty";
    case NN_ExprFnBlock:
	return "NN_ExprFnBlock";
    case NN_ExprAgain:
	return "NN_ExprAgain";
    case NN_components:
	return "NN_components";
    case NN_ItemEnum:
	return "NN_ItemEnum";
    case NN_ExprBox:
	return "NN_ExprBox";
    case NN_ItemTy:
	return "NN_ItemTy";
    case NN_NONE:
	return "NN_NONE";
    case NN_SHL:
	return "NN_SHL";
    case NN_SHR:
	return "NN_SHR";
    case NN_LE:
	return "NN_LE";
    case NN_EQEQ:
	return "NN_EQEQ";
    case NN_NE:
	return "NN_NE";
    case NN_GE:
	return "NN_GE";
    case NN_ANDAND:
	return "NN_ANDAND";
    case NN_OROR:
	return "NN_OROR";
    case NN_LARROW:
	return "NN_LARROW";
    case NN_SHLEQ:
	return "NN_SHLEQ";
    case NN_SHREQ:
	return "NN_SHREQ";
    case NN_MINUSEQ:
	return "NN_MINUSEQ";
    case NN_ANDEQ:
	return "NN_ANDEQ";
    case NN_OREQ:
	return "NN_OREQ";
    case NN_PLUSEQ:
	return "NN_PLUSEQ";
    case NN_STAREQ:
	return "NN_STAREQ";
    case NN_SLASHEQ:
	return "NN_SLASHEQ";
    case NN_CARETEQ:
	return "NN_CARETEQ";
    case NN_PERCENTEQ:
	return "NN_PERCENTEQ";
    case NN_DOTDOT:
	return "NN_DOTDOT";
    case NN_DOTDOTDOT:
	return "NN_DOTDOTDOT";
    case NN_MOD_SEP:
	return "NN_MOD_SEP";
    case NN_RARROW:
	return "NN_RARROW";
    case NN_FAT_ARROW:
	return "NN_FAT_ARROW";
    case NN_LIT_BYTE:
	return "NN_LIT_BYTE";
    case NN_LIT_CHAR:
	return "NN_LIT_CHAR";
    case NN_LIT_INTEGER:
	return "NN_LIT_INTEGER";
    case NN_LIT_FLOAT:
	return "NN_LIT_FLOAT";
    case NN_LIT_STR:
	return "NN_LIT_STR";
    case NN_LIT_STR_RAW:
	return "NN_LIT_STR_RAW";
    case NN_LIT_BYTE_STR:
	return "NN_LIT_BYTE_STR";
    case NN_LIT_BYTE_STR_RAW:
	return "NN_LIT_BYTE_STR_RAW";
    case NN_IDENT:
	return "NN_IDENT";
    case NN_UNDERSCORE:
	return "NN_UNDERSCORE";
    case NN_LIFETIME:
	return "NN_LIFETIME";
    case NN_SELF:
	return "NN_SELF";
    case NN_STATIC:
	return "NN_STATIC";
    case NN_ABSTRACT:
	return "NN_ABSTRACT";
    case NN_ALIGNOF:
	return "NN_ALIGNOF";
    case NN_AS:
	return "NN_AS";
    case NN_BECOME:
	return "NN_BECOME";
    case NN_BREAK:
	return "NN_BREAK";
    case NN_CATCH:
	return "NN_CATCH";
    case NN_CRATE:
	return "NN_CRATE";
    case NN_DEFAULT:
	return "NN_DEFAULT";
    case NN_DO:
	return "NN_DO";
    case NN_ELSE:
	return "NN_ELSE";
    case NN_ENUM:
	return "NN_ENUM";
    case NN_EXTERN:
	return "NN_EXTERN";
    case NN_XFALSE:
	return "NN_XFALSE";
    case NN_FINAL:
	return "NN_FINAL";
    case NN_FN:
	return "NN_FN";
    case NN_FOR:
	return "NN_FOR";
    case NN_IF:
	return "NN_IF";
    case NN_IMPL:
	return "NN_IMPL";
    case NN_IN:
	return "NN_IN";
    case NN_LET:
	return "NN_LET";
    case NN_LOOP:
	return "NN_LOOP";
    case NN_MACRO:
	return "NN_MACRO";
    case NN_MATCH:
	return "NN_MATCH";
    case NN_MOD:
	return "NN_MOD";
    case NN_MOVE:
	return "NN_MOVE";
    case NN_MUT:
	return "NN_MUT";
    case NN_OFFSETOF:
	return "NN_OFFSETOF";
    case NN_OVERRIDE:
	return "NN_OVERRIDE";
    case NN_PRIV:
	return "NN_PRIV";
    case NN_PUB:
	return "NN_PUB";
    case NN_PURE:
	return "NN_PURE";
    case NN_REF:
	return "NN_REF";
    case NN_RETURN:
	return "NN_RETURN";
    case NN_STRUCT:
	return "NN_STRUCT";
    case NN_SIZEOF:
	return "NN_SIZEOF";
    case NN_SUPER:
	return "NN_SUPER";
    case NN_XTRUE:
	return "NN_XTRUE";
    case NN_TRAIT:
	return "NN_TRAIT";
    case NN_TYPE:
	return "NN_TYPE";
    case NN_UNION:
	return "NN_UNION";
    case NN_UNSAFE:
	return "NN_UNSAFE";
    case NN_UNSIZED:
	return "NN_UNSIZED";
    case NN_USE:
	return "NN_USE";
    case NN_VIRTUAL:
	return "NN_VIRTUAL";
    case NN_WHILE:
	return "NN_WHILE";
    case NN_YIELD:
	return "NN_YIELD";
    case NN_CONTINUE:
	return "NN_CONTINUE";
    case NN_PROC:
	return "NN_PROC";
    case NN_BOX:
	return "NN_BOX";
    case NN_CONST:
	return "NN_CONST";
    case NN_WHERE:
	return "NN_WHERE";
    case NN_TYPEOF:
	return "NN_TYPEOF";
    case NN_INNER_DOC_COMMENT:
	return "NN_INNER_DOC_COMMENT";
    case NN_OUTER_DOC_COMMENT:
	return "NN_OUTER_DOC_COMMENT";
    case NN_SHEBANG:
	return "NN_SHEBANG";
    case NN_STATIC_LIFETIME:
	return "NN_STATIC_LIFETIME";
    case NN_SEMI_COLON:
	return "NN_SEMI_COLON";
    case NN_COMMA:
	return "NN_COMMA";
    case NN_SINGLE_DOT:
	return "NN_SINGLE_DOT";
    case NN_AT:
	return "NN_AT";
    case NN_HASH:
	return "NN_HASH";
    case NN_TILDA:
	return "NN_TILDA";
    case NN_COLON:
	return "NN_COLON";
    case NN_DOLLAR:
	return "NN_DOLLAR";
    case NN_EQUALS:
	return "NN_EQUALS";
    case NN_QUESTION:
	return "NN_QUESTION";
    case NN_EXCLAIM:
	return "NN_EXCLAIM";
    case NN_LESS_THAN:
	return "NN_LESS_THAN";
    case NN_GREATER_THAN:
	return "NN_GREATER_THAN";
    case NN_MINUS:
	return "NN_MINUS";
    case NN_AMPERSAND:
	return "NN_AMPERSAND";
    case NN_PIPE:
	return "NN_PIPE";
    case NN_PLUS:
	return "NN_PLUS";
    case NN_MULT:
	return "NN_MULT";
    case NN_DIVIDE:
	return "NN_DIVIDE";
    case NN_HAT:
	return "NN_HAT";
    case NN_PERCENTAGE:
	return "NN_PERCENTAGE";
    case NN_GLOBAL:
        return "NN_GLOBAL";
    case NN_stmts:
        return "NN_stmts";
    case NN_BiOr:
        return "NN_BiOr";
    case NN_BiAnd:
        return "NN_BiAnd";
    case NN_BiEq:
        return "NN_BiEq";
    case NN_BiNe:
        return "NN_BiNe";
    case NN_BiLt:
        return "NN_BiLt";
    case NN_BiGt:
        return "NN_BiGt";
    case NN_BiLe:
        return "NN_BiLe";
    case NN_BiGe:
        return "NN_BiGe";
    case NN_BiBitOr:
        return "NN_BiBitOr";
    case NN_BiBitXor:
        return "NN_BiBitXor";
    case NN_BiBitAnd:
        return "NN_BiBitAnd";
    case NN_BiShl:
        return "NN_BiShl";
    case NN_BiShr:
        return "NN_BiShr";
    case NN_BiAdd:
        return "NN_BiAdd";
    case NN_BiSub:
        return "NN_BiSub";
    case NN_BiMul:
        return "NN_BiMul";
    case NN_BiDiv:
        return "NN_BiDiv";
    case NN_BiRem:
        return "NN_BiRem";
    case NN_UnNeg:
        return "NN_UnNeg";
    case NN_UnNot:
        return "NN_UnNot";
    case NN_UnDeref:
        return "NN_UnDeref";
    }
    return NULL;
}
