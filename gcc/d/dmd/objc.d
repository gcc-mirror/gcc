/**
 * Interfacing with Objective-C.
 *
 * Specification: $(LINK2 https://dlang.org/spec/objc_interface.html, Interfacing to Objective-C)
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/objc.d, _objc.d)
 * Documentation:  https://dlang.org/phobos/dmd_objc.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/objc.d
 */

module dmd.objc;

import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.attrib;
import dmd.attribsem;
import dmd.cond;
import dmd.dclass;
import dmd.declaration;
import dmd.denum;
import dmd.dmodule;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.errors;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.globals;
import dmd.gluelayer;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.mangle;
import dmd.mtype;
import dmd.root.array;
import dmd.common.outbuffer;
import dmd.root.stringtable;
import dmd.target;
import dmd.tokens;

struct ObjcSelector
{
    // MARK: Selector
    private __gshared StringTable!(ObjcSelector*) stringtable;
    private __gshared int incnum = 0;
    const(char)* stringvalue;
    size_t stringlen;
    size_t paramCount;

    extern (C++) static void _init()
    {
        stringtable._init();
    }

    extern (D) this(const(char)* sv, size_t len, size_t pcount) @safe
    {
        stringvalue = sv;
        stringlen = len;
        paramCount = pcount;
    }

    extern (D) static ObjcSelector* lookup(const(char)* s)
    {
        size_t len = 0;
        size_t pcount = 0;
        const(char)* i = s;
        while (*i != 0)
        {
            ++len;
            if (*i == ':')
                ++pcount;
            ++i;
        }
        return lookup(s, len, pcount);
    }

    extern (D) static ObjcSelector* lookup(const(char)* s, size_t len, size_t pcount)
    {
        auto sv = stringtable.update(s, len);
        ObjcSelector* sel = sv.value;
        if (!sel)
        {
            sel = new ObjcSelector(sv.toDchars(), len, pcount);
            sv.value = sel;
        }
        return sel;
    }

    static const(char)[] toPascalCase(const(char)[] id) {
        OutBuffer buf;
        char firstChar = id[0];
        if (firstChar >= 'a' && firstChar <= 'z')
            firstChar = cast(char)(firstChar - 'a' + 'A');

        buf.writeByte(firstChar);
        buf.writestring(id[1..$]);
        return cast(const(char)[])buf.extractSlice(false);
    }

    extern (C++) static ObjcSelector* create(FuncDeclaration fdecl)
    {
        OutBuffer buf;
        auto ftype = cast(TypeFunction)fdecl.type;
        const id = fdecl.ident.toString();
        const nparams = ftype.parameterList.length;

        // Special case: property setter
        if (ftype.isProperty && nparams == 1)
        {

            // Special case: "isXYZ:"
            if (id.length >= 2 && id[0..2] == "is")
            {
                buf.writestring("set");
                buf.write(toPascalCase(id[2..$]));
            }
            else
            {
                buf.writestring("set");
                buf.write(toPascalCase(id));
            }
            buf.writeByte(':');
            goto Lcomplete;
        }

        // write identifier in selector
        buf.write(id[]);

        // To make it easier to match the selectors of objects nicely,
        // the implementation has been replaced so that the parameter name followed by a colon
        // is used instead.
        // eg. void myFunction(int a, int b, int c) would be mangled to a selector as `myFunction:b:c:
        if (nparams > 1)
        {
            buf.writeByte(':');
            foreach(i; 1..nparams)
            {
                buf.write(ftype.parameterList[i].ident.toString());
                buf.writeByte(':');
            }
        }
        else if (nparams == 1)
        {
            buf.writeByte(':');
        }
    Lcomplete:
        buf.writeByte('\0');

        // the slice is not expected to include a terminating 0
        return lookup(cast(const(char)*)buf[].ptr, buf.length - 1, nparams);
    }

    extern (D) const(char)[] toString() const pure
    {
        return stringvalue[0 .. stringlen];
    }
}

private __gshared Objc _objc;

Objc objc()
{
    return _objc;
}


/**
 * Contains all data for a class declaration that is needed for the Objective-C
 * integration.
 */
extern (C++) struct ObjcClassDeclaration
{
    /// `true` if this class is a metaclass.
    bool isMeta = false;

    /// `true` if this class is externally defined.
    bool isExtern = false;

    /// Name of this class.
    Identifier identifier;

    /// The class declaration this belongs to.
    ClassDeclaration classDeclaration;

    /// The metaclass of this class.
    ClassDeclaration metaclass;

    /// List of non-inherited methods.
    FuncDeclaration[] methodList;

    extern (D) this(ClassDeclaration classDeclaration) @safe
    {
        this.classDeclaration = classDeclaration;
    }

    bool isRootClass() const @safe
    {
        return classDeclaration.classKind == ClassKind.objc &&
            !metaclass &&
            !classDeclaration.baseClass;
    }
}

/**
 * Contains all data for a function declaration that is needed for the
 * Objective-C integration.
 */
extern (C++) struct ObjcFuncDeclaration
{
    /// The method selector (member functions only).
    ObjcSelector* selector;

    /// The implicit selector parameter.
    VarDeclaration selectorParameter;

    /// `true` if this function declaration is declared optional.
    bool isOptional;
}

// Should be an interface
extern(C++) abstract class Objc
{
    static void _init()
    {
        if (target.objc.supported)
            _objc = new Supported;
        else
            _objc = new Unsupported;
    }

    /**
     * Deinitializes the global state of the compiler.
     *
     * This can be used to restore the state set by `_init` to its original
     * state.
     */
    static void deinitialize()
    {
        _objc = _objc.init;
    }

    abstract void setObjc(ClassDeclaration cd);
    abstract void setObjc(InterfaceDeclaration);

    /**
     * Returns a pretty textual representation of the given class declaration.
     *
     * Params:
     *  classDeclaration = the class declaration to return the textual representation for
     *  qualifyTypes = `true` if types should be qualified in the result
     *
     * Returns: the textual representation
     */
    abstract const(char)* toPrettyChars(ClassDeclaration classDeclaration, bool qualifyTypes) const;

    abstract void setSelector(FuncDeclaration, Scope* sc);
    abstract void validateSelector(FuncDeclaration fd);
    abstract void checkLinkage(FuncDeclaration fd);

    /**
     * Returns `true` if the given function declaration is virtual.
     *
     * Function declarations with Objective-C linkage and which are static or
     * final are considered virtual.
     *
     * Params:
     *  fd = the function declaration to check if it's virtual
     *
     * Returns: `true` if the given function declaration is virtual
     */
    abstract bool isVirtual(const FuncDeclaration fd) const;

    /**
     * Marks the given function declaration as optional.
     *
     * A function declaration is considered optional if it's annotated with the
     * UDA: `@(core.attribute.optional)`. Only function declarations inside
     * interface declarations and with Objective-C linkage can be declared as
     * optional.
     *
     * Params:
     *  functionDeclaration = the function declaration to be set as optional
     *  sc = the scope from the semantic phase
     */
    abstract void setAsOptional(FuncDeclaration functionDeclaration, Scope* sc) const;

    /**
     * Validates function declarations declared optional.
     *
     * Params:
     *  functionDeclaration = the function declaration to validate
     */
    abstract void validateOptional(FuncDeclaration functionDeclaration) const;

    /**
     * Gets the parent of the given function declaration.
     *
     * Handles Objective-C static member functions, which are virtual functions
     * of the metaclass, by returning the parent class declaration to the
     * metaclass.
     *
     * Params:
     *  fd = the function declaration to get the parent of
     *  cd = the current parent, i.e. the class declaration the given function
     *      declaration belongs to
     *
     * Returns: the parent
     */
    abstract ClassDeclaration getParent(FuncDeclaration fd,
        ClassDeclaration cd) const;

    /**
     * Adds the given function to the list of Objective-C methods.
     *
     * This list will later be used output the necessary Objective-C module info.
     *
     * Params:
     *  fd = the function declaration to be added to the list
     *  cd = the class declaration the function belongs to
     */
    abstract void addToClassMethodList(FuncDeclaration fd,
        ClassDeclaration cd) const;

    /**
     * Returns the `this` pointer of the given function declaration.
     *
     * This is only used for class/static methods. For instance methods, no
     * Objective-C specialization is necessary.
     *
     * Params:
     *  funcDeclaration = the function declaration to get the `this` pointer for
     *
     * Returns: the `this` pointer of the given function declaration, or `null`
     *  if the given function declaration is not an Objective-C method.
     */
    abstract inout(AggregateDeclaration) isThis(inout FuncDeclaration funcDeclaration) const;

    /**
     * Creates the selector parameter for the given function declaration.
     *
     * Objective-C methods has an extra hidden parameter that comes after the
     * `this` parameter. The selector parameter is of the Objective-C type `SEL`
     * and contains the selector which this method was called with.
     *
     * Params:
     *  fd = the function declaration to create the parameter for
     *  sc = the scope from the semantic phase
     *
     * Returns: the newly created selector parameter or `null` for
     *  non-Objective-C functions
     */
    abstract VarDeclaration createSelectorParameter(FuncDeclaration fd, Scope* sc) const;

    /**
     * Creates and sets the metaclass on the given class/interface declaration.
     *
     * Will only be performed on regular Objective-C classes, not on metaclasses.
     *
     * Params:
     *  classDeclaration = the class/interface declaration to set the metaclass on
     */
    abstract void setMetaclass(InterfaceDeclaration interfaceDeclaration, Scope* sc) const;

    /// ditto
    abstract void setMetaclass(ClassDeclaration classDeclaration, Scope* sc) const;

    /**
     * Returns Objective-C runtime metaclass of the given class declaration.
     *
     * `ClassDeclaration.ObjcClassDeclaration.metaclass` contains the metaclass
     * from the semantic point of view. This function returns the metaclass from
     * the Objective-C runtime's point of view. Here, the metaclass of a
     * metaclass is the root metaclass, not `null`. The root metaclass's
     * metaclass is itself.
     *
     * Params:
     *  classDeclaration = The class declaration to return the metaclass of
     *
     * Returns: the Objective-C runtime metaclass of the given class declaration
     */
    abstract ClassDeclaration getRuntimeMetaclass(ClassDeclaration classDeclaration) const;

    ///
    abstract void addSymbols(AttribDeclaration attribDeclaration,
        ClassDeclarations* classes, ClassDeclarations* categories) const;

    ///
    abstract void addSymbols(ClassDeclaration classDeclaration,
        ClassDeclarations* classes, ClassDeclarations* categories) const;

    /**
     * Issues a compile time error if the `.offsetof`/`.tupleof` property is
     * used on a field of an Objective-C class.
     *
     * To solve the fragile base class problem in Objective-C, fields have a
     * dynamic offset instead of a static offset. The compiler outputs a
     * statically known offset which later the dynamic loader can update, if
     * necessary, when the application is loaded. Due to this behavior it
     * doesn't make sense to be able to get the offset of a field at compile
     * time, because this offset might not actually be the same at runtime.
     *
     * To get the offset of a field that is correct at runtime, functionality
     * from the Objective-C runtime can be used instead.
     *
     * Params:
     *  expression = the `.offsetof`/`.tupleof` expression
     *  aggregateDeclaration = the aggregate declaration the field of the
     *      `.offsetof`/`.tupleof` expression belongs to
     *  type = the type of the receiver of the `.tupleof` expression
     *
     * See_Also:
     *  $(LINK2 https://en.wikipedia.org/wiki/Fragile_binary_interface_problem,
     *      Fragile Binary Interface Problem)
     *
     * See_Also:
     *  $(LINK2 https://developer.apple.com/documentation/objectivec/objective_c_runtime,
     *      Objective-C Runtime)
     */
    abstract void checkOffsetof(Expression expression, AggregateDeclaration aggregateDeclaration) const;

    /// ditto
    abstract void checkTupleof(Expression expression, TypeClass type) const;
}

extern(C++) private final class Unsupported : Objc
{
    extern(D) final this()
    {
        ObjcGlue.initialize();
    }

    override void setObjc(ClassDeclaration cd)
    {
        .error(cd.loc, "%s `%s` Objective-C classes not supported", cd.kind, cd.toPrettyChars);
    }

    override void setObjc(InterfaceDeclaration id)
    {
        .error(id.loc, "%s `%s` Objective-C interfaces not supported", id.kind, id.toPrettyChars);
    }

    override const(char)* toPrettyChars(ClassDeclaration, bool qualifyTypes) const
    {
        assert(0, "Should never be called when Objective-C is not supported");
    }

    override void setSelector(FuncDeclaration, Scope*)
    {
        // noop
    }

    override void validateSelector(FuncDeclaration)
    {
        // noop
    }

    override void checkLinkage(FuncDeclaration)
    {
        // noop
    }

    override bool isVirtual(const FuncDeclaration) const
    {
        assert(0, "Should never be called when Objective-C is not supported");
    }

    override void setAsOptional(FuncDeclaration, Scope*) const
    {
        // noop
    }

    override void validateOptional(FuncDeclaration) const
    {
        // noop
    }

    override ClassDeclaration getParent(FuncDeclaration, ClassDeclaration cd) const
    {
        return cd;
    }

    override void addToClassMethodList(FuncDeclaration, ClassDeclaration) const
    {
        // noop
    }

    override inout(AggregateDeclaration) isThis(inout FuncDeclaration funcDeclaration) const
    {
        return null;
    }

    override VarDeclaration createSelectorParameter(FuncDeclaration, Scope*) const
    {
        return null;
    }

    override void setMetaclass(InterfaceDeclaration, Scope*) const
    {
        // noop
    }

    override void setMetaclass(ClassDeclaration, Scope*) const
    {
        // noop
    }

    override ClassDeclaration getRuntimeMetaclass(ClassDeclaration classDeclaration) const
    {
        assert(0, "Should never be called when Objective-C is not supported");
    }

    override void addSymbols(AttribDeclaration attribDeclaration,
        ClassDeclarations* classes, ClassDeclarations* categories) const
    {
        // noop
    }

    override void addSymbols(ClassDeclaration classDeclaration,
        ClassDeclarations* classes, ClassDeclarations* categories) const
    {
        // noop
    }

    override void checkOffsetof(Expression expression, AggregateDeclaration aggregateDeclaration) const
    {
        // noop
    }

    override void checkTupleof(Expression expression, TypeClass type) const
    {
        // noop
    }
}

extern(C++) private final class Supported : Objc
{
    extern(D) final this()
    {
        VersionCondition.addPredefinedGlobalIdent("D_ObjectiveC");

        ObjcGlue.initialize();
        ObjcSelector._init();
    }

    override void setObjc(ClassDeclaration cd)
    {
        cd.classKind = ClassKind.objc;
        cd.objc.isExtern = (cd.storage_class & STC.extern_) > 0;
    }

    override void setObjc(InterfaceDeclaration id)
    {
        id.classKind = ClassKind.objc;
        id.objc.isExtern = true;
    }

    override const(char)* toPrettyChars(ClassDeclaration cd, bool qualifyTypes) const
    {
        return cd.parent.toPrettyChars(qualifyTypes);
    }

    override void setSelector(FuncDeclaration fd, Scope* sc)
    {
        foreachUda(fd, sc, (e) {
            if (!e.isStructLiteralExp())
                return 0;

            auto literal = e.isStructLiteralExp();
            assert(literal.sd);

            if (!isCoreUda(literal.sd, Id.udaSelector))
                return 0;

            if (fd.objc.selector)
            {
                .error(fd.loc, "%s `%s` can only have one Objective-C selector per method", fd.kind, fd.toPrettyChars);
                return 1;
            }

            assert(literal.elements.length == 1);
            auto se = (*literal.elements)[0].toStringExp();
            assert(se);

            fd.objc.selector = ObjcSelector.lookup(se.toUTF8(sc).peekString().ptr);

            return 0;
        });

        // Avoid attempting to generate selectors for template instances.
        if (fd.parent && fd.parent.isTemplateInstance())
            return;

        // No selector declared, generate one.
        if (fd._linkage == LINK.objc && !fd.objc.selector)
        {
            fd.objc.selector = ObjcSelector.create(fd);
        }
    }

    override void validateSelector(FuncDeclaration fd)
    {
        if (!fd.objc.selector)
            return;
        auto tf = cast(TypeFunction)fd.type;
        if (fd.objc.selector.paramCount != tf.parameterList.parameters.length)
            .error(fd.loc, "%s `%s` number of colons in Objective-C selector must match number of parameters", fd.kind, fd.toPrettyChars);
        if (fd.parent && fd.parent.isTemplateInstance())
            .error(fd.loc, "%s `%s` template cannot have an Objective-C selector attached", fd.kind, fd.toPrettyChars);
    }

    override void checkLinkage(FuncDeclaration fd)
    {
        if (fd._linkage != LINK.objc && fd.objc.selector)
            .error(fd.loc, "%s `%s` must have Objective-C linkage to attach a selector", fd.kind, fd.toPrettyChars);
    }

    override bool isVirtual(const FuncDeclaration fd) const
    in
    {
        assert(fd.selector);
        assert(fd.isMember);
    }
    do
    {
        if (fd.toParent.isInterfaceDeclaration && fd.isFinal)
            return false;

        // * final member functions are kept virtual with Objective-C linkage
        //   because the Objective-C runtime always use dynamic dispatch.
        // * static member functions are kept virtual too, as they represent
        //   methods of the metaclass.
        with (fd.visibility)
            return !(kind == Visibility.Kind.private_ || kind == Visibility.Kind.package_);
    }

    override void setAsOptional(FuncDeclaration fd, Scope* sc) const
    {
        const count = declaredAsOptionalCount(fd, sc);
        fd.objc.isOptional = count > 0;

        if (count > 1)
            .error(fd.loc, "%s `%s` can only declare a function as optional once", fd.kind, fd.toPrettyChars);
    }

    /// Returns: the number of times `fd` has been declared as optional.
    private int declaredAsOptionalCount(FuncDeclaration fd , Scope* sc) const
    {
        int count;

        foreachUda(fd, sc, (e) {
            if (!e.isTypeExp())
                return 0;

            auto typeExp = e.isTypeExp();

            if (typeExp.type.ty != Tenum)
                return 0;

            auto typeEnum = cast(TypeEnum) typeExp.type;

            if (isCoreUda(typeEnum.sym, Id.udaOptional))
                count++;

            return 0;
        });

        return count;
    }

    override void validateOptional(FuncDeclaration fd) const
    {
        if (!fd.objc.isOptional)
            return;

        if (fd._linkage != LINK.objc)
        {
            .error(fd.loc, "%s `%s` only functions with Objective-C linkage can be declared as optional", fd.kind, fd.toPrettyChars);

            const linkage = linkageToString(fd._linkage);

            errorSupplemental(fd.loc, "function is declared with %.*s linkage",
                cast(uint) linkage.length, linkage.ptr);
        }

        auto parent = fd.parent;

        if (parent && parent.isTemplateInstance())
        {
            .error(fd.loc, "%s `%s` template cannot be optional", fd.kind, fd.toPrettyChars);
            parent = parent.parent;
            assert(parent);
        }

        if (parent && !parent.isInterfaceDeclaration())
        {
            .error(fd.loc, "%s `%s` only functions declared inside interfaces can be optional", fd.kind, fd.toPrettyChars);
            errorSupplemental(fd.loc, "function is declared inside %s", fd.parent.kind);
        }
    }

    override ClassDeclaration getParent(FuncDeclaration fd, ClassDeclaration cd) const
    out(metaclass)
    {
        assert(metaclass);
    }
    do
    {
        if (cd.classKind == ClassKind.objc && fd.isStatic && !cd.objc.isMeta)
            return cd.objc.metaclass;
        else
            return cd;
    }

    override void addToClassMethodList(FuncDeclaration fd, ClassDeclaration cd) const
    in
    {
        assert(fd.parent.isClassDeclaration);
    }
    do
    {
        if (cd.classKind != ClassKind.objc)
            return;

        if (!fd.objc.selector)
            return;

        assert(fd.isStatic ? cd.objc.isMeta : !cd.objc.isMeta);

        cd.objc.methodList ~= fd;
    }

    override inout(AggregateDeclaration) isThis(inout FuncDeclaration funcDeclaration) const
    {
        with(funcDeclaration)
        {
            if (!objc.selector)
                return null;

            // Use Objective-C class object as 'this'
            auto cd = isMember2().isClassDeclaration();

            if (cd.classKind == ClassKind.objc)
            {
                if (!cd.objc.isMeta)
                    return cd.objc.metaclass;
            }

            return null;
        }
    }

    override VarDeclaration createSelectorParameter(FuncDeclaration fd, Scope* sc) const
    in
    {
        assert(fd.selectorParameter is null);
    }
    do
    {
        if (!fd.objc.selector)
            return null;

        auto ident = Identifier.generateAnonymousId("_cmd");
        auto var = new VarDeclaration(fd.loc, Type.tvoidptr, ident, null);
        var.storage_class |= STC.parameter;
        var.dsymbolSemantic(sc);
        if (!sc.insert(var))
            assert(false);
        var.parent = fd;

        return var;
    }

    override void setMetaclass(InterfaceDeclaration interfaceDeclaration, Scope* sc) const
    {
        auto newMetaclass(Loc loc, BaseClasses* metaBases)
        {
            auto ident = createMetaclassIdentifier(interfaceDeclaration);
            return new InterfaceDeclaration(loc, ident, metaBases);
        }

        .setMetaclass!newMetaclass(interfaceDeclaration, sc);
    }

    override void setMetaclass(ClassDeclaration classDeclaration, Scope* sc) const
    {
        auto newMetaclass(Loc loc, BaseClasses* metaBases)
        {
            auto ident = createMetaclassIdentifier(classDeclaration);
            return new ClassDeclaration(loc, ident, metaBases, new Dsymbols(), 0);
        }

        .setMetaclass!newMetaclass(classDeclaration, sc);
    }

    override ClassDeclaration getRuntimeMetaclass(ClassDeclaration classDeclaration) const
    {
        if (!classDeclaration.objc.metaclass && classDeclaration.objc.isMeta)
        {
            if (classDeclaration.baseClass)
                return getRuntimeMetaclass(classDeclaration.baseClass);
            else
                return classDeclaration;
        }
        else
            return classDeclaration.objc.metaclass;
    }

    override void addSymbols(AttribDeclaration attribDeclaration,
        ClassDeclarations* classes, ClassDeclarations* categories) const
    {
        auto symbols = attribDeclaration.include(null);

        if (!symbols)
            return;

        foreach (symbol; *symbols)
            symbol.addObjcSymbols(classes, categories);
    }

    override void addSymbols(ClassDeclaration classDeclaration,
        ClassDeclarations* classes, ClassDeclarations* categories) const
    {
        with (classDeclaration)
            if (classKind == ClassKind.objc && !objc.isExtern && !objc.isMeta)
                classes.push(classDeclaration);
    }

    override void checkOffsetof(Expression expression, AggregateDeclaration aggregateDeclaration) const
    {
        if (aggregateDeclaration.classKind != ClassKind.objc)
            return;

        enum errorMessage = "no property `offsetof` for member `%s` of type " ~
            "`%s`";

        enum supplementalMessage = "`offsetof` is not available for members " ~
            "of Objective-C classes. Please use the Objective-C runtime instead";

        error(expression.loc, errorMessage, expression.toChars(),
            expression.type.toChars());
        errorSupplemental(expression.loc, supplementalMessage);
    }

    override void checkTupleof(Expression expression, TypeClass type) const
    {
        if (type.sym.classKind != ClassKind.objc)
            return;

        error(expression.loc, "no property `tupleof` for type `%s`", type.toChars());
        errorSupplemental(expression.loc, "`tupleof` is not available for members " ~
            "of Objective-C classes. Please use the Objective-C runtime instead");
    }
}

/*
 * Creates and sets the metaclass on the given class/interface declaration.
 *
 * Will only be performed on regular Objective-C classes, not on metaclasses.
 *
 * Params:
 *  newMetaclass = a function that returns the metaclass to set. This should
 *      return the same type as `T`.
 *  classDeclaration = the class/interface declaration to set the metaclass on
 */
private void setMetaclass(alias newMetaclass, T)(T classDeclaration, Scope* sc)
if (is(T == ClassDeclaration) || is(T == InterfaceDeclaration))
{
    static if (is(T == ClassDeclaration))
        enum errorType = "class";
    else
        enum errorType = "interface";

    with (classDeclaration)
    {
        if (classKind != ClassKind.objc || objc.isMeta || objc.metaclass)
            return;

        if (!objc.identifier)
            objc.identifier = classDeclaration.ident;

        auto metaBases = new BaseClasses();

        foreach (base ; baseclasses.opSlice)
        {
            auto baseCd = base.sym;
            assert(baseCd);

            if (baseCd.classKind == ClassKind.objc)
            {
                assert(baseCd.objc.metaclass);
                assert(baseCd.objc.metaclass.objc.isMeta);
                assert(baseCd.objc.metaclass.type.ty == Tclass);

                auto metaBase = new BaseClass(baseCd.objc.metaclass.type);
                metaBase.sym = baseCd.objc.metaclass;
                metaBases.push(metaBase);
            }
            else
            {
                .error(classDeclaration.loc, "%s `%s` base " ~ errorType ~ " for an Objective-C " ~
                      errorType ~ " must be `extern (Objective-C)`", classDeclaration.kind, classDeclaration.toPrettyChars);
            }
        }

        objc.metaclass = newMetaclass(loc, metaBases);
        objc.metaclass.storage_class |= STC.static_;
        objc.metaclass.classKind = ClassKind.objc;
        objc.metaclass.objc.isMeta = true;
        objc.metaclass.objc.isExtern = objc.isExtern;
        objc.metaclass.objc.identifier = objc.identifier;

        if (baseClass)
            objc.metaclass.baseClass = baseClass.objc.metaclass;

        members.push(objc.metaclass);
        objc.metaclass.addMember(sc, classDeclaration);

        objc.metaclass.members = new Dsymbols();
        objc.metaclass.dsymbolSemantic(sc);
    }
}

private Identifier createMetaclassIdentifier(ClassDeclaration classDeclaration)
{
    const name = "class_" ~ classDeclaration.ident.toString ~ "_Meta";
    return Identifier.generateAnonymousId(name);
}
