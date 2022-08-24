/**
 * Defines a visitor for the AST.
 *
 * Other visitors derive from this class.
 *
 * Documentation:  https://dlang.org/phobos/dmd_parsetimevisitor.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/parsetimevisitor.d
 */

module dmd.parsetimevisitor;

/** Basic and dumm visitor which implements a visit method for each AST node
  * implemented in AST. This visitor is the parent of strict, transitive
  * and permissive visitors.
  */
extern (C++) class ParseTimeVisitor(AST)
{
public:
    void visit(AST.Dsymbol) { assert(0); }
    void visit(AST.Parameter) { assert(0); }
    void visit(AST.Statement) { assert(0); }
    void visit(AST.Type) { assert(0); }
    void visit(AST.Expression) { assert(0); }
    void visit(AST.TemplateParameter) { assert(0); }
    void visit(AST.Condition) { assert(0); }
    void visit(AST.Initializer) { assert(0); }

    //=======================================================================================
    // Dsymbols
    void visit(AST.AliasThis s) { visit(cast(AST.Dsymbol)s); }
    void visit(AST.Declaration s) { visit(cast(AST.Dsymbol)s); }
    void visit(AST.ScopeDsymbol s) { visit(cast(AST.Dsymbol)s); }
    void visit(AST.Import s) { visit(cast(AST.Dsymbol)s); }
    void visit(AST.AttribDeclaration s) { visit(cast(AST.Dsymbol)s); }
    void visit(AST.StaticAssert s) { visit(cast(AST.Dsymbol)s); }
    void visit(AST.DebugSymbol s) { visit(cast(AST.Dsymbol)s); }
    void visit(AST.VersionSymbol s) { visit(cast(AST.Dsymbol)s); }
    void visit(AST.AliasAssign s) { visit(cast(AST.Dsymbol)s); }

    // ScopeDsymbols
    void visit(AST.Package s) { visit(cast(AST.ScopeDsymbol)s); }
    void visit(AST.EnumDeclaration s) { visit(cast(AST.ScopeDsymbol)s); }
    void visit(AST.AggregateDeclaration s) { visit(cast(AST.ScopeDsymbol)s); }
    void visit(AST.TemplateDeclaration s) { visit(cast(AST.ScopeDsymbol)s); }
    void visit(AST.TemplateInstance s) { visit(cast(AST.ScopeDsymbol)s); }
    void visit(AST.Nspace s) { visit(cast(AST.ScopeDsymbol)s); }

    //=========================================================================================
    // Declarations
    void visit(AST.VarDeclaration s) { visit(cast(AST.Declaration)s); }
    void visit(AST.FuncDeclaration s) { visit(cast(AST.Declaration)s); }
    void visit(AST.AliasDeclaration s) { visit(cast(AST.Declaration)s); }
    void visit(AST.TupleDeclaration s) { visit(cast(AST.Declaration)s); }

    // FuncDeclarations
    void visit(AST.FuncLiteralDeclaration s) { visit(cast(AST.FuncDeclaration)s); }
    void visit(AST.PostBlitDeclaration s) { visit(cast(AST.FuncDeclaration)s); }
    void visit(AST.CtorDeclaration s) { visit(cast(AST.FuncDeclaration)s); }
    void visit(AST.DtorDeclaration s) { visit(cast(AST.FuncDeclaration)s); }
    void visit(AST.InvariantDeclaration s) { visit(cast(AST.FuncDeclaration)s); }
    void visit(AST.UnitTestDeclaration s) { visit(cast(AST.FuncDeclaration)s); }
    void visit(AST.NewDeclaration s) { visit(cast(AST.FuncDeclaration)s); }
    void visit(AST.StaticCtorDeclaration s) { visit(cast(AST.FuncDeclaration)s); }
    void visit(AST.StaticDtorDeclaration s) { visit(cast(AST.FuncDeclaration)s); }
    void visit(AST.SharedStaticCtorDeclaration s) { visit(cast(AST.StaticCtorDeclaration)s); }
    void visit(AST.SharedStaticDtorDeclaration s) { visit(cast(AST.StaticDtorDeclaration)s); }

    // AttribDeclarations
    void visit(AST.CompileDeclaration s) { visit(cast(AST.AttribDeclaration)s); }
    void visit(AST.UserAttributeDeclaration s) { visit(cast(AST.AttribDeclaration)s); }
    void visit(AST.LinkDeclaration s) { visit(cast(AST.AttribDeclaration)s); }
    void visit(AST.AnonDeclaration s) { visit(cast(AST.AttribDeclaration)s); }
    void visit(AST.AlignDeclaration s) { visit(cast(AST.AttribDeclaration)s); }
    void visit(AST.CPPMangleDeclaration s) { visit(cast(AST.AttribDeclaration)s); }
    void visit(AST.CPPNamespaceDeclaration s) { visit(cast(AST.AttribDeclaration)s); }
    void visit(AST.VisibilityDeclaration s) { visit(cast(AST.AttribDeclaration)s); }
    void visit(AST.PragmaDeclaration s) { visit(cast(AST.AttribDeclaration)s); }
    void visit(AST.StorageClassDeclaration s) { visit(cast(AST.AttribDeclaration)s); }
    void visit(AST.ConditionalDeclaration s) { visit(cast(AST.AttribDeclaration)s); }
    void visit(AST.StaticForeachDeclaration s) { visit(cast(AST.AttribDeclaration)s); }

    //==============================================================================================
    // Miscellaneous
    void visit(AST.DeprecatedDeclaration s) { visit(cast(AST.StorageClassDeclaration)s); }
    void visit(AST.StaticIfDeclaration s) { visit(cast(AST.ConditionalDeclaration)s); }
    void visit(AST.EnumMember s) { visit(cast(AST.VarDeclaration)s); }
    void visit(AST.Module s) { visit(cast(AST.Package)s); }
    void visit(AST.StructDeclaration s) { visit(cast(AST.AggregateDeclaration)s); }
    void visit(AST.UnionDeclaration s) { visit(cast(AST.StructDeclaration)s); }
    void visit(AST.ClassDeclaration s) { visit(cast(AST.AggregateDeclaration)s); }
    void visit(AST.InterfaceDeclaration s) { visit(cast(AST.ClassDeclaration)s); }
    void visit(AST.TemplateMixin s) { visit(cast(AST.TemplateInstance)s); }
    void visit(AST.BitFieldDeclaration s) { visit(cast(AST.VarDeclaration)s); }

    //============================================================================================
    // Statements
    void visit(AST.ImportStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.ScopeStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.ReturnStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.LabelStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.StaticAssertStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.CompileStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.WhileStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.ForStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.DoStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.ForeachRangeStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.ForeachStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.IfStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.ScopeGuardStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.ConditionalStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.StaticForeachStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.PragmaStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.SwitchStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.CaseRangeStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.CaseStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.DefaultStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.BreakStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.ContinueStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.GotoDefaultStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.GotoCaseStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.GotoStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.SynchronizedStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.WithStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.TryCatchStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.TryFinallyStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.ThrowStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.AsmStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.ExpStatement s) { visit(cast(AST.Statement)s); }
    void visit(AST.CompoundStatement s) { visit(cast(AST.Statement)s); }

    // CompoundStatements
    void visit(AST.CompoundDeclarationStatement s) { visit(cast(AST.CompoundStatement)s); }
    void visit(AST.CompoundAsmStatement s) { visit(cast(AST.CompoundStatement)s); }

    // AsmStatements
    void visit(AST.InlineAsmStatement s) { visit(cast(AST.AsmStatement)s); }
    void visit(AST.GccAsmStatement s) { visit(cast(AST.AsmStatement)s); }

    //=========================================================================================
    // Types
    void visit(AST.TypeBasic t) { visit(cast(AST.Type)t); }
    void visit(AST.TypeError t) { visit(cast(AST.Type)t); }
    void visit(AST.TypeNull t) { visit(cast(AST.Type)t); }
    void visit(AST.TypeNoreturn t) { visit(cast(AST.Type)t); }
    void visit(AST.TypeVector t) { visit(cast(AST.Type)t); }
    void visit(AST.TypeEnum t) { visit(cast(AST.Type)t); }
    void visit(AST.TypeTuple t) { visit(cast(AST.Type)t); }
    void visit(AST.TypeClass t) { visit(cast(AST.Type)t); }
    void visit(AST.TypeStruct t) { visit(cast(AST.Type)t); }
    void visit(AST.TypeNext t) { visit(cast(AST.Type)t); }
    void visit(AST.TypeQualified t) { visit(cast(AST.Type)t); }
    void visit(AST.TypeTraits t) { visit(cast(AST.Type)t); }
    void visit(AST.TypeMixin t) { visit(cast(AST.Type)t); }
    void visit(AST.TypeTag t) { visit(cast(AST.Type)t); }

    // TypeNext
    void visit(AST.TypeReference t) { visit(cast(AST.TypeNext)t); }
    void visit(AST.TypeSlice t) { visit(cast(AST.TypeNext)t); }
    void visit(AST.TypeDelegate t) { visit(cast(AST.TypeNext)t); }
    void visit(AST.TypePointer t) { visit(cast(AST.TypeNext)t); }
    void visit(AST.TypeFunction t) { visit(cast(AST.TypeNext)t); }
    void visit(AST.TypeArray t) { visit(cast(AST.TypeNext)t); }

    // TypeArray
    void visit(AST.TypeDArray t) { visit(cast(AST.TypeArray)t); }
    void visit(AST.TypeAArray t) { visit(cast(AST.TypeArray)t); }
    void visit(AST.TypeSArray t) { visit(cast(AST.TypeArray)t); }

    // TypeQualified
    void visit(AST.TypeIdentifier t) { visit(cast(AST.TypeQualified)t); }
    void visit(AST.TypeReturn t) { visit(cast(AST.TypeQualified)t); }
    void visit(AST.TypeTypeof t) { visit(cast(AST.TypeQualified)t); }
    void visit(AST.TypeInstance t) { visit(cast(AST.TypeQualified)t); }

    //=================================================================================
    // Expressions
    void visit(AST.DeclarationExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.IntegerExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.NewAnonClassExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.IsExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.RealExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.NullExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.TypeidExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.TraitsExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.StringExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.NewExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.AssocArrayLiteralExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.ArrayLiteralExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.MixinExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.FuncExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.IntervalExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.TypeExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.ScopeExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.IdentifierExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.UnaExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.DefaultInitExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.BinExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.DsymbolExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.TemplateExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.SymbolExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.TupleExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.ThisExp e) { visit(cast(AST.Expression)e); }
    void visit(AST.GenericExp e) { visit(cast(AST.Expression)e); }

    // Miscellaneous
    void visit(AST.VarExp e) { visit(cast(AST.SymbolExp)e); }
    void visit(AST.DollarExp e) { visit(cast(AST.IdentifierExp)e); }
    void visit(AST.SuperExp e) { visit(cast(AST.ThisExp)e); }

    // UnaExp
    void visit(AST.AddrExp e) { visit(cast(AST.UnaExp)e); }
    void visit(AST.PreExp e) { visit(cast(AST.UnaExp)e); }
    void visit(AST.PtrExp e) { visit(cast(AST.UnaExp)e); }
    void visit(AST.NegExp e) { visit(cast(AST.UnaExp)e); }
    void visit(AST.UAddExp e) { visit(cast(AST.UnaExp)e); }
    void visit(AST.NotExp e) { visit(cast(AST.UnaExp)e); }
    void visit(AST.ComExp e) { visit(cast(AST.UnaExp)e); }
    void visit(AST.DeleteExp e) { visit(cast(AST.UnaExp)e); }
    void visit(AST.CastExp e) { visit(cast(AST.UnaExp)e); }
    void visit(AST.CallExp e) { visit(cast(AST.UnaExp)e); }
    void visit(AST.DotIdExp e) { visit(cast(AST.UnaExp)e); }
    void visit(AST.AssertExp e) { visit(cast(AST.UnaExp)e); }
    void visit(AST.ThrowExp e) { visit(cast(AST.UnaExp)e); }
    void visit(AST.ImportExp e) { visit(cast(AST.UnaExp)e); }
    void visit(AST.DotTemplateInstanceExp e) { visit(cast(AST.UnaExp)e); }
    void visit(AST.ArrayExp e) { visit(cast(AST.UnaExp)e); }

    // DefaultInitExp
    void visit(AST.FuncInitExp e) { visit(cast(AST.DefaultInitExp)e); }
    void visit(AST.PrettyFuncInitExp e) { visit(cast(AST.DefaultInitExp)e); }
    void visit(AST.FileInitExp e) { visit(cast(AST.DefaultInitExp)e); }
    void visit(AST.LineInitExp e) { visit(cast(AST.DefaultInitExp)e); }
    void visit(AST.ModuleInitExp e) { visit(cast(AST.DefaultInitExp)e); }

    // BinExp
    void visit(AST.CommaExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.PostExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.PowExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.MulExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.DivExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.ModExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.AddExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.MinExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.CatExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.ShlExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.ShrExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.UshrExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.EqualExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.InExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.IdentityExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.CmpExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.AndExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.XorExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.OrExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.LogicalExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.CondExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.AssignExp e) { visit(cast(AST.BinExp)e); }
    void visit(AST.BinAssignExp e) { visit(cast(AST.BinExp)e); }

    // BinAssignExp
    void visit(AST.AddAssignExp e) { visit(cast(AST.BinAssignExp)e); }
    void visit(AST.MinAssignExp e) { visit(cast(AST.BinAssignExp)e); }
    void visit(AST.MulAssignExp e) { visit(cast(AST.BinAssignExp)e); }
    void visit(AST.DivAssignExp e) { visit(cast(AST.BinAssignExp)e); }
    void visit(AST.ModAssignExp e) { visit(cast(AST.BinAssignExp)e); }
    void visit(AST.PowAssignExp e) { visit(cast(AST.BinAssignExp)e); }
    void visit(AST.AndAssignExp e) { visit(cast(AST.BinAssignExp)e); }
    void visit(AST.OrAssignExp e) { visit(cast(AST.BinAssignExp)e); }
    void visit(AST.XorAssignExp e) { visit(cast(AST.BinAssignExp)e); }
    void visit(AST.ShlAssignExp e) { visit(cast(AST.BinAssignExp)e); }
    void visit(AST.ShrAssignExp e) { visit(cast(AST.BinAssignExp)e); }
    void visit(AST.UshrAssignExp e) { visit(cast(AST.BinAssignExp)e); }
    void visit(AST.CatAssignExp e) { visit(cast(AST.BinAssignExp)e); }

    // CatAssignExp
    void visit(AST.CatElemAssignExp e) { visit(cast(AST.CatAssignExp)e); }
    void visit(AST.CatDcharAssignExp e) { visit(cast(AST.CatAssignExp)e); }

    //===============================================================================
    // TemplateParameter
    void visit(AST.TemplateAliasParameter tp) { visit(cast(AST.TemplateParameter)tp); }
    void visit(AST.TemplateTypeParameter tp) { visit(cast(AST.TemplateParameter)tp); }
    void visit(AST.TemplateTupleParameter tp) { visit(cast(AST.TemplateParameter)tp); }
    void visit(AST.TemplateValueParameter tp) { visit(cast(AST.TemplateParameter)tp); }

    void visit(AST.TemplateThisParameter tp) { visit(cast(AST.TemplateTypeParameter)tp); }

    //===============================================================================
    // Condition
    void visit(AST.StaticIfCondition c) { visit(cast(AST.Condition)c); }
    void visit(AST.DVCondition c) { visit(cast(AST.Condition)c); }
    void visit(AST.DebugCondition c) { visit(cast(AST.DVCondition)c); }
    void visit(AST.VersionCondition c) { visit(cast(AST.DVCondition)c); }

    //===============================================================================
    // Initializer
    void visit(AST.ExpInitializer i) { visit(cast(AST.Initializer)i); }
    void visit(AST.StructInitializer i) { visit(cast(AST.Initializer)i); }
    void visit(AST.ArrayInitializer i) { visit(cast(AST.Initializer)i); }
    void visit(AST.VoidInitializer i) { visit(cast(AST.Initializer)i); }
    void visit(AST.CInitializer i) { visit(cast(AST.CInitializer)i); }
}
