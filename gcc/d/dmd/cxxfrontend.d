/**
 * Contains C++ interfaces for interacting with DMD as a library.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/cxxfrontend.d, _cxxfrontend.d)
 * Documentation:  https://dlang.org/phobos/dmd_cxxfrontend.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/cxxfrontend.d
 */
module dmd.cxxfrontend;

import dmd.aggregate : AggregateDeclaration;
import dmd.arraytypes;
import dmd.astenums;
import dmd.common.outbuffer : OutBuffer;
import dmd.denum : EnumDeclaration;
import dmd.dmodule /*: Module*/;
import dmd.dscope : Scope;
import dmd.dstruct /*: StructDeclaration*/;
import dmd.dsymbol : Dsymbol, ScopeDsymbol, CAsmDeclaration, SearchOpt, SearchOptFlags;
import dmd.dtemplate /*: TemplateInstance, TemplateParameter, Tuple*/;
import dmd.errorsink : ErrorSink;
import dmd.expression /*: Expression*/;
import dmd.func : FuncDeclaration;
import dmd.globals;
import dmd.identifier : Identifier;
import dmd.init : Initializer, NeedInterpret;
import dmd.location : Loc;
import dmd.mtype /*: Covariant, Type, Parameter, ParameterList*/;
import dmd.rootobject : RootObject;
import dmd.statement : Statement, AsmStatement, GccAsmStatement;

// NB: At some point in the future, we can switch to shortened function syntax.
extern (C++, "dmd"):

/***********************************************************
 * cppmangle.d
 */
const(char)* toCppMangleItanium(Dsymbol s)
{
    import dmd.cppmangle;
    return dmd.cppmangle.toCppMangleItanium(s);
}

const(char)* cppTypeInfoMangleItanium(Dsymbol s)
{
    import dmd.cppmangle;
    return dmd.cppmangle.cppTypeInfoMangleItanium(s);
}

const(char)* cppThunkMangleItanium(FuncDeclaration fd, int offset)
{
    import dmd.cppmangle;
    return dmd.cppmangle.cppThunkMangleItanium(fd, offset);
}

/***********************************************************
 * dinterpret.d
 */
Expression ctfeInterpret(Expression e)
{
    import dmd.dinterpret;
    return dmd.dinterpret.ctfeInterpret(e);
}

/***********************************************************
 * dmangle.d
 */
const(char)* mangleExact(FuncDeclaration fd)
{
    import dmd.dmangle;
    return dmd.dmangle.mangleExact(fd);
}

void mangleToBuffer(Type t, ref OutBuffer buf)
{
    import dmd.dmangle;
    return dmd.dmangle.mangleToBuffer(t, buf);
}

void mangleToBuffer(Expression e, ref OutBuffer buf)
{
    import dmd.dmangle;
    return dmd.dmangle.mangleToBuffer(e, buf);
}

void mangleToBuffer(Dsymbol s, ref OutBuffer buf)
{
    import dmd.dmangle;
    return dmd.dmangle.mangleToBuffer(s, buf);
}

void mangleToBuffer(TemplateInstance ti, ref OutBuffer buf)
{
    import dmd.dmangle;
    return dmd.dmangle.mangleToBuffer(ti, buf);
}

/***********************************************************
 * dmodule.d
 */
void getLocalClasses(Module mod, ref ClassDeclarations aclasses)
{
    return dmd.dmodule.getLocalClasses(mod, aclasses);
}

FuncDeclaration findGetMembers(ScopeDsymbol dsym)
{
    return dmd.dmodule.findGetMembers(dsym);
}

/***********************************************************
 * doc.d
 */
void gendocfile(Module m, const char* ddoctext_ptr, size_t ddoctext_length,
                const char* datetime, ErrorSink eSink, ref OutBuffer outbuf)
{
    import dmd.doc;
    return dmd.doc.gendocfile(m, ddoctext_ptr, ddoctext_length, datetime, eSink, outbuf);
}

/***********************************************************
 * dstruct.d
 */
FuncDeclaration search_toString(StructDeclaration sd)
{
    return dmd.dstruct.search_toString(sd);
}

/***********************************************************
 * dsymbolsem.d
 */
void dsymbolSemantic(Dsymbol dsym, Scope* sc)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.dsymbolSemantic(dsym, sc);
}

void addMember(Dsymbol dsym, Scope* sc, ScopeDsymbol sds)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.addMember(dsym, sc, sds);
}

Dsymbol search(Dsymbol d, const ref Loc loc, Identifier ident, SearchOptFlags
               flags = SearchOpt.all)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.search(d, loc, ident, flags);
}

void setScope(Dsymbol d, Scope* sc)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.setScope(d, sc);
}

void importAll(Dsymbol d, Scope* sc)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.importAll(d, sc);
}

/***********************************************************
 * dtemplate.d
 */
inout(Expression) isExpression(inout RootObject o)
{
    return dmd.dtemplate.isExpression(o);
}

inout(Dsymbol) isDsymbol(inout RootObject o)
{
    return dmd.dtemplate.isDsymbol(o);
}

inout(Type) isType(inout RootObject o)
{
    return dmd.dtemplate.isType(o);
}

inout(Tuple) isTuple(inout RootObject o)
{
    return dmd.dtemplate.isTuple(o);
}

inout(Parameter) isParameter(inout RootObject o)
{
    return dmd.dtemplate.isParameter(o);
}

inout(TemplateParameter) isTemplateParameter(inout RootObject o)
{
    return dmd.dtemplate.isTemplateParameter(o);
}

bool isError(const RootObject o)
{
    return dmd.dtemplate.isError(o);
}

void printTemplateStats(bool listInstances, ErrorSink eSink)
{
    return dmd.dtemplate.printTemplateStats(listInstances, eSink);
}

/***********************************************************
 * dtoh.d
 */
void genCppHdrFiles(ref Modules ms)
{
    import dmd.dtoh;
    return dmd.dtoh.genCppHdrFiles(ms);
}

/***********************************************************
 * enumsem.d
 */
Expression getDefaultValue(EnumDeclaration ed, const ref Loc loc)
{
    import dmd.enumsem;
    return dmd.enumsem.getDefaultValue(ed, loc);
}

/***********************************************************
 * expression.d
 */
void expandTuples(Expressions* exps, Identifiers* names = null)
{
    return dmd.expression.expandTuples(exps, names);
}

/***********************************************************
 * expressionsem.d
 */
Expression expressionSemantic(Expression e, Scope* sc)
{
    import dmd.expressionsem;
    return dmd.expressionsem.expressionSemantic(e, sc);
}

/***********************************************************
 * funcsem.d
 */
bool functionSemantic(FuncDeclaration fd)
{
    import dmd.funcsem;
    return dmd.funcsem.functionSemantic(fd);
}

bool functionSemantic3(FuncDeclaration fd)
{
    import dmd.funcsem;
    return dmd.funcsem.functionSemantic3(fd);
}

/***********************************************************
 * hdrgen.d
 */
void genhdrfile(Module m, bool doFuncBodies, ref OutBuffer buf)
{
    import dmd.hdrgen;
    return dmd.hdrgen.genhdrfile(m, doFuncBodies, buf);
}

const(char)* toChars(const Statement s)
{
    import dmd.hdrgen;
    return dmd.hdrgen.toChars(s);
}

const(char)* toChars(const Expression e)
{
    import dmd.hdrgen;
    return dmd.hdrgen.toChars(e);
}

const(char)* toChars(const Initializer i)
{
    import dmd.hdrgen;
    return dmd.hdrgen.toChars(i);
}

const(char)* toChars(const Type t)
{
    import dmd.hdrgen;
    return dmd.hdrgen.toChars(t);
}

void moduleToBuffer(ref OutBuffer buf, bool vcg_ast, Module m)
{
    import dmd.hdrgen;
    return dmd.hdrgen.moduleToBuffer(buf, vcg_ast, m);
}

const(char)* parametersTypeToChars(ParameterList pl)
{
    import dmd.hdrgen;
    return dmd.hdrgen.parametersTypeToChars(pl);
}

/***********************************************************
 * iasm.d
 */
Statement asmSemantic(AsmStatement s, Scope *sc)
{
    import dmd.iasm;
    return dmd.iasm.asmSemantic(s, sc);
}

void asmSemantic(CAsmDeclaration d, Scope *sc)
{
    import dmd.iasm;
    return dmd.iasm.asmSemantic(d, sc);
}

/***********************************************************
 * iasmgcc.d
 */
Statement gccAsmSemantic(GccAsmStatement s, Scope *sc)
{
    import dmd.iasmgcc;
    return dmd.iasmgcc.gccAsmSemantic(s, sc);
}

void gccAsmSemantic(CAsmDeclaration d, Scope *sc)
{
    import dmd.iasmgcc;
    return dmd.iasmgcc.gccAsmSemantic(d, sc);
}

/***********************************************************
 * initsem.d
 */
Initializer initializerSemantic(Initializer init, Scope* sc, ref Type tx,
                                NeedInterpret needInterpret)
{
    import dmd.initsem;
    return dmd.initsem.initializerSemantic(init, sc, tx, needInterpret);
}

Expression initializerToExpression(Initializer init, Type itype = null, const
                                   bool isCfile = false)
{
    import dmd.initsem;
    return dmd.initsem.initializerToExpression(init, itype, isCfile);
}

/***********************************************************
 * json.d
 */
void json_generate(ref Modules modules, ref OutBuffer buf)
{
    import dmd.json;
    return dmd.json.json_generate(modules, buf);
}

JsonFieldFlags tryParseJsonField(const(char)* fieldName)
{
    import dmd.json;
    return dmd.json.tryParseJsonField(fieldName);
}

/***********************************************************
 * mtype.d
 */
AggregateDeclaration isAggregate(Type t)
{
    return dmd.mtype.isAggregate(t);
}

/***********************************************************
 * optimize.d
 */
Expression optimize(Expression e, int result, bool keepLvalue = false)
{
    import dmd.optimize;
    return dmd.optimize.optimize(e, result, keepLvalue);
}

/***********************************************************
 * semantic2.d
 */
void semantic2(Dsymbol dsym, Scope* sc)
{
    import dmd.semantic2;
    return dmd.semantic2.semantic2(dsym, sc);
}

/***********************************************************
 * semantic3.d
 */
void semantic3(Dsymbol dsym, Scope* sc)
{
    import dmd.semantic3;
    return dmd.semantic3.semantic3(dsym, sc);
}

void semanticTypeInfoMembers(StructDeclaration sd)
{
    import dmd.semantic3;
    return dmd.semantic3.semanticTypeInfoMembers(sd);
}

/***********************************************************
 * statementsem.d
 */
Statement statementSemantic(Statement s, Scope* sc)
{
    import dmd.statementsem;
    return dmd.statementsem.statementSemantic(s, sc);
}

/***********************************************************
 * templateparamsem.d
 */
bool tpsemantic(TemplateParameter tp, Scope* sc, TemplateParameters* parameters)
{
    import dmd.templateparamsem;
    return dmd.templateparamsem.tpsemantic(tp, sc, parameters);
}

/***********************************************************
 * typesem.d
 */
bool hasPointers(Type t)
{
    import dmd.typesem;
    return dmd.typesem.hasPointers(t);
}

Type typeSemantic(Type type, const ref Loc loc, Scope* sc)
{
    import dmd.typesem;
    return dmd.typesem.typeSemantic(type, loc, sc);
}

Type trySemantic(Type type, const ref Loc loc, Scope* sc)
{
    import dmd.typesem;
    return dmd.typesem.trySemantic(type, loc, sc);
}

Type merge(Type type)
{
    import dmd.typesem;
    return dmd.typesem.merge(type);
}

Type merge2(Type type)
{
    import dmd.typesem;
    return dmd.typesem.merge2(type);
}

Expression defaultInit(Type mt, const ref Loc loc, const bool isCfile = false)
{
    import dmd.typesem;
    return dmd.typesem.defaultInit(mt, loc, isCfile);
}

Dsymbol toDsymbol(Type type, Scope* sc)
{
    import dmd.typesem;
    return dmd.typesem.toDsymbol(type, sc);
}

Covariant covariant(Type src, Type t, StorageClass* pstc = null, bool
                    cppCovariant = false)
{
    import dmd.typesem;
    return dmd.typesem.covariant(src, t, pstc, cppCovariant);
}

bool isBaseOf(Type tthis, Type t, int* poffset)
{
    import dmd.typesem;
    return dmd.typesem.isBaseOf(tthis, t, poffset);
}

bool equivalent(Type src, Type t)
{
    import dmd.typesem;
    return dmd.typesem.equivalent(src, t);
}

Type sarrayOf(Type type, dinteger_t dim)
{
    import dmd.typesem;
    return dmd.typesem.sarrayOf(type, dim);
}

Type arrayOf(Type type)
{
    import dmd.typesem;
    return dmd.typesem.arrayOf(type);
}

Type constOf(Type type)
{
    import dmd.typesem;
    return dmd.typesem.constOf(type);
}

Type immutableOf(Type type)
{
    import dmd.typesem;
    return dmd.typesem.immutableOf(type);
}

Type mutableOf(Type type)
{
    import dmd.typesem;
    return dmd.typesem.mutableOf(type);
}

Type sharedOf(Type type)
{
    import dmd.typesem;
    return dmd.typesem.sharedOf(type);
}

Type sharedConstOf(Type type)
{
    import dmd.typesem;
    return dmd.typesem.sharedConstOf(type);
}

Type unSharedOf(Type type)
{
    import dmd.typesem;
    return dmd.typesem.unSharedOf(type);
}

Type wildOf(Type type)
{
    import dmd.typesem;
    return dmd.typesem.wildOf(type);
}

Type wildConstOf(Type type)
{
    import dmd.typesem;
    return dmd.typesem.wildConstOf(type);
}

Type sharedWildOf(Type type)
{
    import dmd.typesem;
    return dmd.typesem.sharedWildOf(type);
}

Type sharedWildConstOf(Type type)
{
    import dmd.typesem;
    return dmd.typesem.sharedWildConstOf(type);
}

Type substWildTo(Type type, uint mod)
{
    import dmd.typesem;
    return dmd.typesem.substWildTo(type, mod);
}

Type unqualify(Type type, uint m)
{
    import dmd.typesem;
    return dmd.typesem.unqualify(type, m);
}

Type toHeadMutable(const(Type) type)
{
    import dmd.typesem;
    return dmd.typesem.toHeadMutable(type);
}

Type aliasthisOf(Type type)
{
    import dmd.typesem;
    return dmd.typesem.aliasthisOf(type);
}

Type castMod(Type type, MOD mod)
{
    import dmd.typesem;
    return dmd.typesem.castMod(type, mod);
}

Type addMod(Type type, MOD mod)
{
    import dmd.typesem;
    return dmd.typesem.addMod(type, mod);
}

Type addStorageClass(Type type, StorageClass stc)
{
    import dmd.typesem;
    return dmd.typesem.addStorageClass(type, stc);
}

Type pointerTo(Type type)
{
    import dmd.typesem;
    return dmd.typesem.pointerTo(type);
}

Type referenceTo(Type type)
{
    import dmd.typesem;
    return dmd.typesem.referenceTo(type);
}

/***********************************************************
 * typinf.d
 */
bool genTypeInfo(Expression e, const ref Loc loc, Type torig, Scope* sc)
{
    import dmd.typinf;
    return dmd.typinf.genTypeInfo(e, loc, torig, sc);
}

bool isSpeculativeType(Type t)
{
    import dmd.typinf;
    return dmd.typinf.isSpeculativeType(t);
}

bool builtinTypeInfo(Type t)
{
    import dmd.typinf;
    return dmd.typinf.builtinTypeInfo(t);
}

version (IN_LLVM)
{
    /***********************************************************
     * argtypes_aarch64.d
     */
    TypeTuple toArgTypes_aarch64(Type t)
    {
        import dmd.argtypes_aarch64;
        return dmd.argtypes_aarch64.toArgTypes_aarch64(t);
    }

    bool isHFVA(Type t, int maxNumElements = 4, Type* rewriteType = null)
    {
        import dmd.argtypes_aarch64;
        return dmd.argtypes_aarch64.isHFVA(t, maxNumElements, rewriteType);
    }

    /***********************************************************
     * argtypes_sysv_x64.d
     */
    TypeTuple toArgTypes_sysv_x64(Type t)
    {
        import dmd.argtypes_sysv_x64;
        return dmd.argtypes_sysv_x64.toArgTypes_sysv_x64(t);
    }

    /***********************************************************
     * argtypes_x86.d
     */
    TypeTuple toArgTypes_x86(Type t)
    {
        import dmd.argtypes_x86;
        return dmd.argtypes_x86.toArgTypes_x86(t);
    }
}
