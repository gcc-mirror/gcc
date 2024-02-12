/**
 * Defines AST nodes for the code generation stage.
 *
 * Documentation:  https://dlang.org/phobos/dmd_astcodegen.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/astcodegen.d
 */
module dmd.astcodegen;


struct ASTCodegen
{
    public import dmd.aggregate;
    public import dmd.aliasthis;
    public import dmd.arraytypes;
    public import dmd.attrib;
    public import dmd.cond;
    public import dmd.dclass;
    public import dmd.declaration;
    public import dmd.denum;
    public import dmd.dimport;
    public import dmd.dmodule;
    public import dmd.dstruct;
    public import dmd.dsymbol;
    public import dmd.dtemplate;
    public import dmd.dversion;
    public import dmd.expression;
    public import dmd.func;
    public import dmd.hdrgen;
    public import dmd.init;
    public import dmd.initsem;
    public import dmd.mtype;
    public import dmd.nspace;
    public import dmd.statement;
    public import dmd.staticassert;
    public import dmd.typesem;
    public import dmd.ctfeexpr;
    public import dmd.init : Designator;


    alias initializerToExpression   = dmd.initsem.initializerToExpression;
    alias typeToExpression          = dmd.typesem.typeToExpression;
    alias UserAttributeDeclaration  = dmd.attrib.UserAttributeDeclaration;
    alias Ensure                    = dmd.func.Ensure; // workaround for bug in older DMD frontends
    alias ErrorExp                  = dmd.expression.ErrorExp;

    alias MODFlags                  = dmd.mtype.MODFlags;
    alias Type                      = dmd.mtype.Type;
    alias Parameter                 = dmd.mtype.Parameter;
    alias Tarray                    = dmd.mtype.Tarray;
    alias Taarray                   = dmd.mtype.Taarray;
    alias Tbool                     = dmd.mtype.Tbool;
    alias Tchar                     = dmd.mtype.Tchar;
    alias Tdchar                    = dmd.mtype.Tdchar;
    alias Tdelegate                 = dmd.mtype.Tdelegate;
    alias Tenum                     = dmd.mtype.Tenum;
    alias Terror                    = dmd.mtype.Terror;
    alias Tfloat32                  = dmd.mtype.Tfloat32;
    alias Tfloat64                  = dmd.mtype.Tfloat64;
    alias Tfloat80                  = dmd.mtype.Tfloat80;
    alias Tfunction                 = dmd.mtype.Tfunction;
    alias Tpointer                  = dmd.mtype.Tpointer;
    alias Treference                = dmd.mtype.Treference;
    alias Tident                    = dmd.mtype.Tident;
    alias Tint8                     = dmd.mtype.Tint8;
    alias Tint16                    = dmd.mtype.Tint16;
    alias Tint32                    = dmd.mtype.Tint32;
    alias Tint64                    = dmd.mtype.Tint64;
    alias Tsarray                   = dmd.mtype.Tsarray;
    alias Tstruct                   = dmd.mtype.Tstruct;
    alias Tuns8                     = dmd.mtype.Tuns8;
    alias Tuns16                    = dmd.mtype.Tuns16;
    alias Tuns32                    = dmd.mtype.Tuns32;
    alias Tuns64                    = dmd.mtype.Tuns64;
    alias Tvoid                     = dmd.mtype.Tvoid;
    alias Twchar                    = dmd.mtype.Twchar;
    alias Tnoreturn                 = dmd.mtype.Tnoreturn;

    alias Timaginary32              = dmd.mtype.Timaginary32;
    alias Timaginary64              = dmd.mtype.Timaginary64;
    alias Timaginary80              = dmd.mtype.Timaginary80;
    alias Tcomplex32                = dmd.mtype.Tcomplex32;
    alias Tcomplex64                = dmd.mtype.Tcomplex64;
    alias Tcomplex80                = dmd.mtype.Tcomplex80;

    alias ModToStc                  = dmd.mtype.ModToStc;
    alias ParameterList             = dmd.mtype.ParameterList;
    alias VarArg                    = dmd.mtype.VarArg;
    alias STC                       = dmd.declaration.STC;
    alias Dsymbol                   = dmd.dsymbol.Dsymbol;
    alias Dsymbols                  = dmd.dsymbol.Dsymbols;
    alias Visibility                = dmd.dsymbol.Visibility;

    alias stcToBuffer               = dmd.hdrgen.stcToBuffer;
    alias linkageToChars            = dmd.hdrgen.linkageToChars;
    alias visibilityToChars         = dmd.hdrgen.visibilityToChars;

    alias isType                    = dmd.dtemplate.isType;
    alias isExpression              = dmd.dtemplate.isExpression;
    alias isTuple                   = dmd.dtemplate.isTuple;

    alias SearchOpt                 = dmd.dsymbol.SearchOpt;
    alias PASS                      = dmd.dsymbol.PASS;
}
