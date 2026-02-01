/**
 * Contains C++ interfaces for interacting with DMD as a library.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/cxxfrontend.d, _cxxfrontend.d)
 * Documentation:  https://dlang.org/phobos/dmd_cxxfrontend.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/cxxfrontend.d
 */
module dmd.cxxfrontend;

import dmd.aggregate : AggregateDeclaration;
import dmd.arraytypes;
import dmd.astenums;
import dmd.attrib;
import dmd.common.outbuffer : OutBuffer;
import dmd.dclass : ClassDeclaration, BaseClass;
import dmd.declaration : TypeInfoDeclaration, VarDeclaration, TupleDeclaration;
import dmd.denum : EnumDeclaration;
import dmd.dmodule /*: Module*/;
import dmd.dscope : Scope;
import dmd.dstruct /*: StructDeclaration*/;
import dmd.dsymbol : Dsymbol, ScopeDsymbol, CAsmDeclaration, SearchOpt, SearchOptFlags;
import dmd.dtemplate /*: TemplateInstance, TemplateParameter, Tuple*/;
import dmd.errorsink : ErrorSink;
import dmd.expression /*: Expression*/;
import dmd.func : FuncDeclaration;
import dmd.globals : dinteger_t, uinteger_t, JsonFieldFlags;
import dmd.identifier : Identifier;
import dmd.init : Initializer, NeedInterpret;
import dmd.location : Loc;
import dmd.mtype /*: Covariant, Type, Parameter, ParameterList*/;
import dmd.rootobject : RootObject;
import dmd.root.optional;
import dmd.root.longdouble : real_t = longdouble;
import dmd.root.complex;
import dmd.semantic3;
import dmd.statement : Statement, AsmStatement, GccAsmStatement;

// NB: At some point in the future, we can switch to shortened function syntax.
extern (C++, "dmd"):

/***********************************************************
 * atrtibsem.d
 */
Expressions* getAttributes(UserAttributeDeclaration a)
{
    import dmd.attribsem;
    return dmd.attribsem.getAttributes(a);
}

/***********************************************************
 * mangle/cpp.d
 */
const(char)* toCppMangleItanium(Dsymbol s)
{
    import dmd.mangle.cpp;
    return dmd.mangle.cpp.toCppMangleItanium(s);
}

const(char)* cppTypeInfoMangleItanium(Dsymbol s)
{
    import dmd.mangle.cpp;
    return dmd.mangle.cpp.cppTypeInfoMangleItanium(s);
}

const(char)* cppThunkMangleItanium(FuncDeclaration fd, int offset)
{
    import dmd.mangle.cpp;
    return dmd.mangle.cpp.cppThunkMangleItanium(fd, offset);
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
 * mangle/package.d
 */
const(char)* mangleExact(FuncDeclaration fd)
{
    import dmd.mangle;
    return dmd.mangle.mangleExact(fd);
}

void mangleToBuffer(Type t, ref OutBuffer buf)
{
    import dmd.mangle;
    return dmd.mangle.mangleToBuffer(t, buf);
}

void mangleToBuffer(Expression e, ref OutBuffer buf)
{
    import dmd.mangle;
    return dmd.mangle.mangleToBuffer(e, buf);
}

void mangleToBuffer(Dsymbol s, ref OutBuffer buf)
{
    import dmd.mangle;
    return dmd.mangle.mangleToBuffer(s, buf);
}

void mangleToBuffer(TemplateInstance ti, ref OutBuffer buf)
{
    import dmd.mangle;
    return dmd.mangle.mangleToBuffer(ti, buf);
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
    return dmd.semantic3.search_toString(sd);
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

Dsymbol search(Dsymbol d, Loc loc, Identifier ident, SearchOptFlags
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

Dsymbols* include(Dsymbol d, Scope* sc)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.include(d, sc);
}

bool isFuncHidden(ClassDeclaration cd, FuncDeclaration fd)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.isFuncHidden(cd, fd);
}

Dsymbol vtblSymbol(ClassDeclaration cd)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.vtblSymbol(cd);
}

bool isAbstract(ClassDeclaration cd)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.isAbstract(cd);
}

bool hasPointers(Dsymbol d)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.hasPointers(d);
}

void getLocalClasses(Module mod, ref ClassDeclarations aclasses)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.getLocalClasses(mod, aclasses);
}

Dsymbol toAlias(Dsymbol s)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.toAlias(s);
}

Dsymbol toAlias2(Dsymbol s)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.toAlias2(s);
}

bool isPOD(StructDeclaration sd)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.isPOD(sd);
}

bool fillVtbl(BaseClass* bc, ClassDeclaration cd, FuncDeclarations* vtbl, int newinstance)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.fillVtbl(bc, cd, vtbl, newinstance);
}

bool overloadInsert(Dsymbol ds, Dsymbol s)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.overloadInsert(ds, s);
}

bool equals(const Dsymbol ds, const Dsymbol s)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.equals(ds, s);
}

Type getType(Dsymbol ds)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.getType(ds);
}

uinteger_t size(Dsymbol ds, Loc loc)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.size(ds, loc);
}

void semantic3OnDependencies(Module m)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.semantic3OnDependencies(m);
}

void addDeferredSemantic(Dsymbol s)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.addDeferredSemantic(s);
}

void addDeferredSemantic2(Dsymbol s)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.addDeferredSemantic2(s);
}

void addDeferredSemantic3(Dsymbol s)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.addDeferredSemantic3(s);
}

void runDeferredSemantic()
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.runDeferredSemantic();
}

void runDeferredSemantic2()
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.runDeferredSemantic2();
}

void runDeferredSemantic3()
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.runDeferredSemantic3();
}

bool isOverlappedWith(VarDeclaration vd, VarDeclaration v)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.isOverlappedWith(vd, v);
}

Scope* newScope(AggregateDeclaration ad, Scope* sc)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.newScope(ad, sc);
}

Dsymbol search(Scope* sc, Loc loc, Identifier ident, out Dsymbol pscopesym,
    SearchOptFlags flags = SearchOpt.all)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.search(sc, loc, ident, pscopesym, flags);
}

void addObjcSymbols(Dsymbol sym, ClassDeclarations* classes, ClassDeclarations* categories)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.addObjcSymbols(sym, classes, categories);
}

FuncDeclaration findGetMembers(ScopeDsymbol dsym)
{
    import dmd.dsymbolsem;
    return dmd.dsymbolsem.findGetMembers(dsym);
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

void printInstantiationTrace(TemplateInstance ti)
{
    return ti.printInstantiationTrace();
}

/***********************************************************
 * dtoh.d
 */
void genCppHdrFiles(ref Modules ms, ErrorSink eSink)
{
    import dmd.dtoh;
    return dmd.dtoh.genCppHdrFiles(ms, eSink);
}

/***********************************************************
 * enumsem.d
 */
Expression getDefaultValue(EnumDeclaration ed, Loc loc)
{
    import dmd.enumsem;
    return dmd.enumsem.getDefaultValue(ed, loc);
}


/***********************************************************
 * expressionsem.d
 */

void expandTuples(Expressions* exps, ArgumentLabels* names = null)
{
    import dmd.expressionsem;
    return dmd.expressionsem.expandTuples(exps, names);
}

Expression expressionSemantic(Expression e, Scope* sc)
{
    import dmd.expressionsem;
    return dmd.expressionsem.expressionSemantic(e, sc);
}

bool fill(StructDeclaration sd, Loc loc,
          ref Expressions elements, bool ctorinit)
{
    import dmd.expressionsem;
    return dmd.expressionsem.fill(sd, loc, elements, ctorinit);
}

bool isIdentical(const Expression exp, const Expression e)
{
    import dmd.expressionsem;
    return dmd.expressionsem.isIdentical(exp, e);
}

bool equals(const Expression exp, const Expression e)
{
    import dmd.expressionsem;
    return dmd.expressionsem.equals(exp, e);
}

bool isLvalue(Expression exp)
{
    import dmd.expressionsem;
    return dmd.expressionsem.isLvalue(exp);
}

int getFieldIndex(ClassReferenceExp cre, Type fieldtype, uint fieldoffset)
{
    import dmd.expressionsem;
    return dmd.expressionsem.getFieldIndex(cre, fieldtype, fieldoffset);
}

void fillTupleExpExps(TupleExp te, TupleDeclaration tup)
{
    import dmd.expressionsem;
    return dmd.expressionsem.fillTupleExpExps(te, tup);
}

Optional!bool toBool(Expression exp)
{
    import dmd.expressionsem;
    return dmd.expressionsem.toBool(exp);
}

StringExp toStringExp(Expression exp)
{
    import dmd.expressionsem;
    return dmd.expressionsem.toStringExp(exp);
}

dinteger_t toInteger(Expression exp)
{
    import dmd.expressionsem;
    return dmd.expressionsem.toInteger(exp);
}

uinteger_t toUInteger(Expression exp)
{
    import dmd.expressionsem;
    return dmd.expressionsem.toUInteger(exp);
}

real_t toReal(Expression exp)
{
    import dmd.expressionsem;
    return dmd.expressionsem.toReal(exp);
}

complex_t toComplex(Expression exp)
{
    import dmd.expressionsem;
    return dmd.expressionsem.toComplex(exp);
}

real_t toImaginary(Expression exp)
{
    import dmd.expressionsem;
    return dmd.expressionsem.toImaginary(exp);
}

/***********************************************************
 * funcsem.d
 */
FuncDeclaration genCfunc(Parameters* fparams, Type treturn, const(char)* name, StorageClass stc = STC.none)
{
    import dmd.funcsem;
    return dmd.funcsem.genCfunc(fparams, treturn, name, cast(STC) stc);
}

FuncDeclaration genCfunc(Parameters* fparams, Type treturn, Identifier id, StorageClass stc = STC.none)
{
    import dmd.funcsem;
    return dmd.funcsem.genCfunc(fparams, treturn, id, cast(STC) stc);
}

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

MATCH leastAsSpecialized(FuncDeclaration fd, FuncDeclaration g, ArgumentLabels* names)
{
    import dmd.funcsem;
    return dmd.funcsem.leastAsSpecialized(fd, g, names);
}

PURE isPure(FuncDeclaration fd)
{
    import dmd.funcsem;
    return dmd.funcsem.isPure(fd);
}

bool needsClosure(FuncDeclaration fd)
{
    import dmd.funcsem;
    return dmd.funcsem.needsClosure(fd);
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
Statement asmSemantic(AsmStatement s, Scope* sc)
{
    import dmd.iasm;
    return dmd.iasm.asmSemantic(s, sc);
}

void asmSemantic(CAsmDeclaration d, Scope* sc)
{
    import dmd.iasm;
    return dmd.iasm.asmSemantic(d, sc);
}

/***********************************************************
 * iasmgcc.d
 */
Statement gccAsmSemantic(GccAsmStatement s, Scope* sc)
{
    import dmd.iasm.gcc;
    return dmd.iasm.gcc.gccAsmSemantic(s, sc);
}

void gccAsmSemantic(CAsmDeclaration d, Scope* sc)
{
    import dmd.iasm.gcc;
    return dmd.iasm.gcc.gccAsmSemantic(d, sc);
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

bool checkClosure(FuncDeclaration fd)
{
    import dmd.semantic3;
    return dmd.semantic3.checkClosure(fd);
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
bool hasDeprecatedAliasThis(Type type)
{
    import dmd.typesem;
    return dmd.typesem.hasDeprecatedAliasThis(type);
}

AggregateDeclaration isAggregate(Type t)
{
    import dmd.typesem;
    return dmd.typesem.isAggregate(t);
}

bool hasPointers(Type t)
{
    import dmd.typesem;
    return dmd.typesem.hasPointers(t);
}

Type typeSemantic(Type type, Loc loc, Scope* sc)
{
    import dmd.typesem;
    return dmd.typesem.typeSemantic(type, loc, sc);
}

Type trySemantic(Type type, Loc loc, Scope* sc)
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

Type toBasetype(Type type)
{
    import dmd.typesem;
    return dmd.typesem.toBasetype(type);
}

Expression defaultInit(Type mt, Loc loc, const bool isCfile = false)
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
    return dmd.typesem.covariant(src, t, cast(STC*) pstc, cppCovariant);
}

bool isZeroInit(Type t, Loc loc)
{
    import dmd.typesem;
    return dmd.typesem.isZeroInit(t, loc);
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
    return dmd.typesem.addStorageClass(type, cast(STC) stc);
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

Type memType(TypeEnum type)
{
    import dmd.typesem;
    return dmd.typesem.memType(type);
}

uinteger_t size(Type type)
{
    import dmd.typesem;
    return dmd.typesem.size(type);
}

uinteger_t size(Type type, Loc loc)
{
    import dmd.typesem;
    return dmd.typesem.size(type, loc);
}

structalign_t alignment(Type type)
{
    import dmd.typesem;
    return dmd.typesem.alignment(type);
}

uint alignsize(Type type)
{
    import dmd.typesem;
    return dmd.typesem.alignsize(type);
}

MATCH implicitConvTo(Type from, Type to)
{
    import dmd.dcast;
    return dmd.dcast.implicitConvTo(from, to);
}

MATCH constConv(Type from, Type to)
{
    import dmd.typesem;
    return dmd.typesem.constConv(from, to);
}

Expression defaultInitLiteral(Type t, Loc loc)
{
    import dmd.typesem;
    return dmd.typesem.defaultInitLiteral(t, loc);
}

bool hasUnsafeBitpatterns(Type type)
{
    import dmd.typesem;
    return dmd.typesem.hasUnsafeBitpatterns(type);
}

bool hasInvariant(Type type)
{
    import dmd.typesem;
    return dmd.typesem.hasInvariant(type);
}

bool hasVoidInitPointers(Type type)
{
    import dmd.typesem;
    return dmd.typesem.hasVoidInitPointers(type);
}

void Type_init()
{
    import dmd.typesem;
    return dmd.typesem.Type_init();
}

void transitive(TypeNext type)
{
    import dmd.typesem;
    return dmd.typesem.transitive(type);
}

Type makeConst(Type type)
{
    import dmd.typesem;
    return dmd.typesem.makeConst(type);
}

Type makeImmutable(Type type)
{
    import dmd.typesem;
    return dmd.typesem.makeImmutable(type);
}

Type makeMutable(Type type)
{
    import dmd.typesem;
    return dmd.typesem.makeMutable(type);
}

Type makeShared(Type type)
{
    import dmd.typesem;
    return dmd.typesem.makeShared(type);
}

Type makeSharedConst(Type type)
{
    import dmd.typesem;
    return dmd.typesem.makeSharedConst(type);
}

Type makeWild(Type type)
{
    import dmd.typesem;
    return dmd.typesem.makeWild(type);
}

Type makeWildConst(Type type)
{
    import dmd.typesem;
    return dmd.typesem.makeWildConst(type);
}

Type makeSharedWild(Type type)
{
    import dmd.typesem;
    return dmd.typesem.makeSharedWild(type);
}

Type makeSharedWildConst(Type type)
{
    import dmd.typesem;
    return dmd.typesem.makeSharedWildConst(type);
}

Type nextOf(Type type)
{
    import dmd.typesem;
    return dmd.typesem.nextOf(type);
}

Type baseElemOf(Type type)
{
    import dmd.typesem;
    return dmd.typesem.baseElemOf(type);
}

Type isLazyArray(Parameter param)
{
    import dmd.typesem;
    return dmd.typesem.isLazyArray(param);
}

MOD deduceWild(Type type, Type t, bool isRef)
{
    import dmd.typesem;
    return dmd.typesem.deduceWild(type, t, isRef);
}

bool isIntegral(Type type)
{
    import dmd.typesem;
    return dmd.typesem.isIntegral(type);
}

bool isFloating(Type type)
{
    import dmd.typesem;
    return dmd.typesem.isFloating(type);
}

bool isScalar(Type type)
{
    import dmd.typesem;
    return dmd.typesem.isScalar(type);
}

bool isReal(Type type)
{
    import dmd.typesem;
    return dmd.typesem.isReal(type);
}

bool isComplex(Type type)
{
    import dmd.typesem;
    return dmd.typesem.isComplex(type);
}

bool isImaginary(Type type)
{
    import dmd.typesem;
    return dmd.typesem.isImaginary(type);
}

bool isString(Type type)
{
    import dmd.typesem;
    return dmd.typesem.isString(type);
}

bool isBoolean(Type type)
{
    import dmd.typesem;
    return dmd.typesem.isBoolean(type);
}

bool isUnsigned(Type type)
{
    import dmd.typesem;
    return dmd.typesem.isUnsigned(type);
}

bool needsNested(Type type)
{
    import dmd.typesem;
    return dmd.typesem.needsNested(type);
}

bool needsDestruction(Type type)
{
    import dmd.typesem;
    return dmd.typesem.needsDestruction(type);
}

bool needsCopyOrPostblit(Type type)
{
    import dmd.typesem;
    return dmd.typesem.needsCopyOrPostblit(type);
}

/***********************************************************
 * typinf.d
 */
bool genTypeInfo(Expression e, Loc loc, Type torig, Scope* sc)
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

Type makeNakedAssociativeArray(TypeAArray t)
{
    import dmd.typinf;
    return dmd.typinf.makeNakedAssociativeArray(t);
}

TypeInfoDeclaration getTypeInfoAssocArrayDeclaration(TypeAArray t, Scope* sc)
{
    import dmd.typinf;
    return dmd.typinf.getTypeInfoAssocArrayDeclaration(t, sc);
}

/**
 * templatesem.d
 */
bool declareParameter(TemplateParameter tp, Scope* sc)
{
    import dmd.templatesem;
    return dmd.templatesem.declareParameter(tp, sc);
}

bool needsCodegen(TemplateInstance ti)
{
    import dmd.templatesem;
    return dmd.templatesem.needsCodegen(ti);
}

bool isDiscardable(TemplateInstance ti)
{
    import dmd.templatesem;
    return dmd.templatesem.isDiscardable(ti);
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
