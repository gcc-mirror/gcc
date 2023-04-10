/**
 * Compile-time checks associated with the @mustuse attribute.
 *
 * Copyright: Copyright (C) 2022-2023 by The D Language Foundation, All Rights Reserved
 * License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/mustuse.d, _mustuse.d)
 * Documentation:  https://dlang.org/phobos/dmd_mustuse.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/mustuse.d
 */

module dmd.mustuse;

import dmd.dscope;
import dmd.dsymbol;
import dmd.expression;
import dmd.globals;
import dmd.identifier;
import dmd.location;

// Used in isIncrementOrDecrement
private static const StringExp plusPlus, minusMinus;

// Loc.initial cannot be used in static initializers, so
// these need a static constructor.
static this()
{
    plusPlus = new StringExp(Loc.initial, "++");
    minusMinus = new StringExp(Loc.initial, "--");
}

/**
 * Check whether discarding an expression would violate the requirements of
 * @mustuse. If so, emit an error.
 *
 * Params:
 *   e = the expression to check
 *   sc = scope in which `e` was semantically analyzed
 *
 * Returns: true on error, false on success.
 */
bool checkMustUse(Expression e, Scope* sc)
{
    import dmd.id : Id;

    assert(e.type);
    if (auto sym = e.type.toDsymbol(sc))
    {
        auto sd = sym.isStructDeclaration();
        // isStructDeclaration returns non-null for both structs and unions
        if (sd && hasMustUseAttribute(sd, sc) && !isAssignment(e) && !isIncrementOrDecrement(e))
        {
            e.error("ignored value of `@%s` type `%s`; prepend a `cast(void)` if intentional",
                Id.udaMustUse.toChars(), e.type.toPrettyChars(true));
            return true;
        }
    }
    return false;
}

/**
 * Called from a symbol's semantic to check for reserved usage of @mustuse.
 *
 * If such usage is found, emits an errror.
 *
 * Params:
 *   sym = symbol to check
 */
void checkMustUseReserved(Dsymbol sym)
{
    import dmd.attrib : foreachUdaNoSemantic;
    import dmd.errors : error;
    import dmd.id : Id;

    // Can't use foreachUda (and by extension hasMustUseAttribute) while
    // semantic analysis of `sym` is still in progress
    foreachUdaNoSemantic(sym, (exp) {
        if (isMustUseAttribute(exp))
        {
            if (sym.isFuncDeclaration())
            {
                error(sym.loc, "`@%s` on functions is reserved for future use",
                    Id.udaMustUse.toChars());
                sym.errors = true;
            }
            else if (sym.isClassDeclaration() || sym.isEnumDeclaration())
            {
                error(sym.loc, "`@%s` on `%s` types is reserved for future use",
                    Id.udaMustUse.toChars(), sym.kind());
                sym.errors = true;
            }
        }
        return 0; // continue
    });
}

/**
 * Returns: true if the given expression is an assignment, either simple (a = b)
 * or compound (a += b, etc).
 */
private bool isAssignment(Expression e)
{
    if (e.isAssignExp || e.isBinAssignExp || e.isConstructExp || e.isBlitExp)
        return true;
    if (auto ce = e.isCallExp())
    {
        if (auto fd = ce.f)
        {
            auto id = fd.ident;
            if (id && isAssignmentOpId(id))
                return true;
        }
    }
    return false;
}

/**
 * Returns: true if id is the identifier of an assignment operator overload.
 */
private bool isAssignmentOpId(Identifier id)
{
    import dmd.id : Id;

    return id == Id.assign
        || id == Id.addass
        || id == Id.subass
        || id == Id.mulass
        || id == Id.divass
        || id == Id.modass
        || id == Id.andass
        || id == Id.orass
        || id == Id.xorass
        || id == Id.shlass
        || id == Id.shrass
        || id == Id.ushrass
        || id == Id.catass
        || id == Id.indexass
        || id == Id.slice
        || id == Id.sliceass
        || id == Id.opOpAssign
        || id == Id.opIndexOpAssign
        || id == Id.opSliceOpAssign
        || id == Id.powass;
}

/**
 * Returns: true if the given expression is an increment (++) or decrement (--).
 */
private bool isIncrementOrDecrement(Expression e)
{
    import dmd.dtemplate : isExpression;
    import dmd.location;
    import dmd.id : Id;
    import dmd.tokens : EXP;

    if (e.op == EXP.plusPlus
        || e.op == EXP.minusMinus
        || e.op == EXP.prePlusPlus
        || e.op == EXP.preMinusMinus)
        return true;
    if (auto call = e.isCallExp())
    {
        // Check for overloaded preincrement
        // e.g., a.opUnary!"++"
        if (auto fd = call.f)
        {
            if (fd.ident == Id.opUnary && fd.parent)
            {
                if (auto ti = fd.parent.isTemplateInstance())
                {
                    auto tiargs = ti.tiargs;
                    if (tiargs && tiargs.length >= 1)
                    {
                        if (auto argExp = (*tiargs)[0].isExpression())
                        {
                            auto op = argExp.isStringExp();
                            if (op && (op.compare(plusPlus) == 0 || op.compare(minusMinus) == 0))
                                return true;
                        }
                    }
                }
            }
        }
    }
    else if (auto comma = e.isCommaExp())
    {
        // Check for overloaded postincrement
        // e.g., (auto tmp = a, ++a, tmp)
        if (comma.e1)
        {
            if (auto left = comma.e1.isCommaExp())
            {
                if (auto middle = left.e2)
                {
                    if (middle && isIncrementOrDecrement(middle))
                        return true;
                }
            }
        }
    }
    return false;
}

/**
 * Returns: true if the given symbol has the @mustuse attribute.
 */
private bool hasMustUseAttribute(Dsymbol sym, Scope* sc)
{
    import dmd.attrib : foreachUda;

    bool result = false;

    foreachUda(sym, sc, (Expression uda) {
        if (isMustUseAttribute(uda))
        {
            result = true;
            return 1; // break
        }
        return 0; // continue
    });

    return result;
}

/**
 * Returns: true if the given expression is core.attribute.mustuse.
 */
private bool isMustUseAttribute(Expression e)
{
    import dmd.attrib : isCoreUda;
    import dmd.id : Id;

    // Logic based on dmd.objc.Supported.declaredAsOptionalCount
    auto typeExp = e.isTypeExp;
    if (!typeExp)
        return false;

    auto typeEnum = typeExp.type.isTypeEnum();
    if (!typeEnum)
        return false;

    if (isCoreUda(typeEnum.sym, Id.udaMustUse))
        return true;

    return false;
}
