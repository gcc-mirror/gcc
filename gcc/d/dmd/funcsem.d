/**
 * Does semantic analysis for functions.
 *
 * Specification: $(LINK2 https://dlang.org/spec/function.html, Functions)
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/funcsem.d, _funcsem.d)
 * Documentation:  https://dlang.org/phobos/dmd_funcsem.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/funcsem.d
 */

module dmd.funcsem;

import core.stdc.stdio;

import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.blockexit;
import dmd.gluelayer;
import dmd.dcast;
import dmd.dclass;
import dmd.declaration;
import dmd.delegatize;
import dmd.dinterpret;
import dmd.dmodule;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.escape;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.location;
import dmd.mtype;
import dmd.objc;
import dmd.root.aav;
import dmd.common.outbuffer;
import dmd.rootobject;
import dmd.root.string;
import dmd.root.stringtable;
import dmd.semantic2;
import dmd.semantic3;
import dmd.statement_rewrite_walker;
import dmd.statement;
import dmd.statementsem;
import dmd.tokens;
import dmd.visitor;

/****************************************************
 * Resolve forward reference of function signature -
 * parameter types, return type, and attributes.
 * Params:
 *  fd = function declaration
 * Returns:
 *  false if any errors exist in the signature.
 */
public
extern (C++)
bool functionSemantic(FuncDeclaration fd)
{
    //printf("functionSemantic() %p %s\n", this, toChars());
    if (!fd._scope)
        return !fd.errors;

    fd.cppnamespace = fd._scope.namespace;

    if (!fd.originalType) // semantic not yet run
    {
        TemplateInstance spec = fd.isSpeculative();
        uint olderrs = global.errors;
        uint oldgag = global.gag;
        if (global.gag && !spec)
            global.gag = 0;
        dsymbolSemantic(fd, fd._scope);
        global.gag = oldgag;
        if (spec && global.errors != olderrs)
            spec.errors = (global.errors - olderrs != 0);
        if (olderrs != global.errors) // if errors compiling this function
            return false;
    }

    // if inferring return type, sematic3 needs to be run
    // - When the function body contains any errors, we cannot assume
    //   the inferred return type is valid.
    //   So, the body errors should become the function signature error.
    if (fd.inferRetType && fd.type && !fd.type.nextOf())
        return fd.functionSemantic3();

    TemplateInstance ti;
    if (fd.isInstantiated() && !fd.isVirtualMethod() &&
        ((ti = fd.parent.isTemplateInstance()) is null || ti.isTemplateMixin() || ti.tempdecl.ident == fd.ident))
    {
        AggregateDeclaration ad = fd.isMemberLocal();
        if (ad && ad.sizeok != Sizeok.done)
        {
            /* Currently dmd cannot resolve forward references per methods,
             * then setting SIZOKfwd is too conservative and would break existing code.
             * So, just stop method attributes inference until ad.dsymbolSemantic() done.
             */
            //ad.sizeok = Sizeok.fwd;
        }
        else
            return fd.functionSemantic3() || !fd.errors;
    }

    if (fd.storage_class & STC.inference)
        return fd.functionSemantic3() || !fd.errors;

    return !fd.errors;
}

/****************************************************
 * Resolve forward reference of function body.
 * Returns false if any errors exist in the body.
 */
public
extern (C++)
bool functionSemantic3(FuncDeclaration fd)
{
    if (fd.semanticRun < PASS.semantic3 && fd._scope)
    {
        /* Forward reference - we need to run semantic3 on this function.
         * If errors are gagged, and it's not part of a template instance,
         * we need to temporarily ungag errors.
         */
        TemplateInstance spec = fd.isSpeculative();
        uint olderrs = global.errors;
        uint oldgag = global.gag;
        if (global.gag && !spec)
            global.gag = 0;
        semantic3(fd, fd._scope);
        global.gag = oldgag;

        // If it is a speculatively-instantiated template, and errors occur,
        // we need to mark the template as having errors.
        if (spec && global.errors != olderrs)
            spec.errors = (global.errors - olderrs != 0);
        if (olderrs != global.errors) // if errors compiling this function
            return false;
    }

    return !fd.errors && !fd.hasSemantic3Errors();
}

// called from semantic3
/**
 * Creates and returns the hidden parameters for this function declaration.
 *
 * Hidden parameters include the `this` parameter of a class, struct or
 * nested function and the selector parameter for Objective-C methods.
 */
extern (D) void declareThis(FuncDeclaration fd, Scope* sc)
{
    const bool dualCtx = (fd.toParent2() != fd.toParentLocal());
    if (dualCtx)
        fd.hasDualContext = true;
    auto ad = fd.isThis();
    if (!dualCtx && !ad && !fd.isNested())
    {
        fd.vthis = null;
        fd.objc.selectorParameter = null;
        return;
    }

    Type addModStc(Type t)
    {
        return t.addMod(fd.type.mod).addStorageClass(fd.storage_class);
    }

    if (dualCtx || fd.isNested())
    {
        /* The 'this' for a nested function is the link to the
         * enclosing function's stack frame.
         * Note that nested functions and member functions are disjoint.
         */
        Type tthis = addModStc(dualCtx ?
                               Type.tvoidptr.sarrayOf(2).pointerTo() :
                               Type.tvoid.pointerTo());
        fd.vthis = new VarDeclaration(fd.loc, tthis, dualCtx ? Id.this2 : Id.capture, null);
        fd.vthis.storage_class |= STC.parameter | STC.nodtor;
    }
    else if (ad)
    {
        Type thandle = addModStc(ad.handleType());
        fd.vthis = new ThisDeclaration(fd.loc, thandle);
        fd.vthis.storage_class |= STC.parameter;
        if (thandle.ty == Tstruct)
        {
            fd.vthis.storage_class |= STC.ref_;
        }
    }

    if (auto tf = fd.type.isTypeFunction())
    {
        if (tf.isreturn)
            fd.vthis.storage_class |= STC.return_;
        if (tf.isScopeQual)
            fd.vthis.storage_class |= STC.scope_;
        if (tf.isreturnscope)
            fd.vthis.storage_class |= STC.returnScope;
    }

    fd.vthis.dsymbolSemantic(sc);
    if (!sc.insert(fd.vthis))
        assert(0);
    fd.vthis.parent = fd;
    if (ad)
        fd.objc.selectorParameter = .objc.createSelectorParameter(fd, sc);
}
