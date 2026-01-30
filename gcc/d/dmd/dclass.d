/**
 * Defines a `class` declaration.
 *
 * Specification: $(LINK2 https://dlang.org/spec/class.html, Classes)
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dclass.d, _dclass.d)
 * Documentation:  https://dlang.org/phobos/dmd_dclass.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/dclass.d
 */

module dmd.dclass;

import core.stdc.stdio;
import core.stdc.string;

import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.gluelayer;
import dmd.declaration;
import dmd.dscope;
import dmd.dsymbol;
import dmd.errors;
import dmd.func;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.objc;
import dmd.root.rmem;
import dmd.target;
import dmd.visitor;

/***********************************************************
 */
extern (C++) struct BaseClass
{
    Type type;          // (before semantic processing)

    ClassDeclaration sym;
    uint offset;        // 'this' pointer offset

    // for interfaces: Array of FuncDeclaration's making up the vtbl[]
    FuncDeclarations vtbl;

    // if BaseClass is an interface, these
    // are a copy of the InterfaceDeclaration.interfaces
    BaseClass[] baseInterfaces;

    extern (D) this(Type type)
    {
        //printf("BaseClass(this = %p, '%s')\n", this, type.toChars());
        this.type = type;
    }

    /****************************************
     * Fill in vtbl[] for base class based on member functions of class cd.
     * Input:
     *      vtbl            if !=NULL, fill it in
     *      newinstance     !=0 means all entries must be filled in by members
     *                      of cd, not members of any base classes of cd.
     * Returns:
     *      true if any entries were filled in by members of cd (not exclusively
     *      by base classes)
     */
    extern (C++) bool fillVtbl(ClassDeclaration cd, FuncDeclarations* vtbl, int newinstance)
    {
        bool result = false;

        //printf("BaseClass.fillVtbl(this='%s', cd='%s')\n", sym.toChars(), cd.toChars());
        if (vtbl)
            vtbl.setDim(sym.vtbl.length);

        // first entry is ClassInfo reference
        for (size_t j = sym.vtblOffset(); j < sym.vtbl.length; j++)
        {
            FuncDeclaration ifd = sym.vtbl[j].isFuncDeclaration();

            //printf("        vtbl[%d] is '%s'\n", j, ifd ? ifd.toChars() : "null");
            assert(ifd);

            // Find corresponding function in this class
            auto tf = ifd.type.toTypeFunction();
            auto fd = cd.findFunc(ifd.ident, tf);
            if (fd && !fd.isAbstract())
            {
                if (fd.toParent() == cd)
                    result = true;
            }
            else
                fd = null;
            if (vtbl)
                (*vtbl)[j] = fd;
        }
        return result;
    }

    extern (D) void copyBaseInterfaces(BaseClasses* vtblInterfaces)
    {
        //printf("+copyBaseInterfaces(), %s\n", sym.toChars());
        //    if (baseInterfaces.length)
        //      return;
        auto bc = cast(BaseClass*)mem.xcalloc(sym.interfaces.length, BaseClass.sizeof);
        baseInterfaces = bc[0 .. sym.interfaces.length];
        //printf("%s.copyBaseInterfaces()\n", sym.toChars());
        for (size_t i = 0; i < baseInterfaces.length; i++)
        {
            BaseClass* b = &baseInterfaces[i];
            BaseClass* b2 = sym.interfaces[i];

            assert(b2.vtbl.length == 0); // should not be filled yet
            memcpy(b, b2, BaseClass.sizeof);

            if (i) // single inheritance is i==0
                vtblInterfaces.push(b); // only need for M.I.
            b.copyBaseInterfaces(vtblInterfaces);
        }
        //printf("-copyBaseInterfaces\n");
    }
}

// These must match the values in druntime/src/object.d
enum ClassFlags : uint
{
    none          = 0x0,
    isCOMclass    = 0x1,
    noPointers    = 0x2,
    hasOffTi      = 0x4,
    hasCtor       = 0x8,
    hasGetMembers = 0x10,
    hasTypeInfo   = 0x20,
    isAbstract    = 0x40,
    isCPPclass    = 0x80,
    hasDtor       = 0x100,
    hasNameSig    = 0x200,
}

/***********************************************************
 */
extern (C++) class ClassDeclaration : AggregateDeclaration
{
    extern (C++) __gshared
    {
        // Names found by reading object.d in druntime
        ClassDeclaration object;
        ClassDeclaration throwable;
        ClassDeclaration exception;
        ClassDeclaration errorException;
        ClassDeclaration cpp_type_info_ptr;   // Object.__cpp_type_info_ptr
    }

    ClassDeclaration baseClass; // NULL only if this is Object
    FuncDeclaration staticCtor;
    FuncDeclaration staticDtor;
    Dsymbols vtbl;              // Array of FuncDeclaration's making up the vtbl[]
    Dsymbols vtblFinal;         // More FuncDeclaration's that aren't in vtbl[]

    // Array of BaseClass's; first is super, rest are Interface's
    BaseClasses* baseclasses;

    /* Slice of baseclasses[] that does not include baseClass
     */
    BaseClass*[] interfaces;

    // array of base interfaces that have their own vtbl[]
    BaseClasses* vtblInterfaces;

    // the ClassInfo object for this ClassDeclaration
    TypeInfoClassDeclaration vclassinfo;

    // true if this is a COM class
    bool com;

    /// true if this is a scope class
    bool stack;

    /// if this is a C++ class, this is the slot reserved for the virtual destructor
    int cppDtorVtblIndex = -1;

    /// to prevent recursive attempts
    bool inuse;

    ThreeState isabstract;

    /// set the progress of base classes resolving
    Baseok baseok;

    /**
     * Data for a class declaration that is needed for the Objective-C
     * integration.
     */
    ObjcClassDeclaration objc;

    Symbol* cpp_type_info_ptr_sym;      // cached instance of class Id.cpp_type_info_ptr

    final extern (D) this(Loc loc, Identifier id, BaseClasses* baseclasses, Dsymbols* members, bool inObject)
    {
        objc = ObjcClassDeclaration(this);

        if (!id)
            id = Identifier.generateAnonymousId("class");

        super(loc, id);
        this.dsym = DSYM.classDeclaration;

        static immutable msg = "only object.d can define this reserved class name";

        if (baseclasses)
        {
            // Actually, this is a transfer
            this.baseclasses = baseclasses;
        }
        else
            this.baseclasses = new BaseClasses();

        this.members = members;

        //printf("ClassDeclaration(%s), dim = %d\n", ident.toChars(), this.baseclasses.length);

        // For forward references
        type = new TypeClass(this);

        // Look for special class names
        if (id == Id.__sizeof || id == Id.__xalignof || id == Id._mangleof)
            classError("%s `%s` illegal class name", null);

        // BUG: What if this is the wrong TypeInfo, i.e. it is nested?
        if (id.toChars()[0] == 'T')
        {
            if (id == Id.TypeInfo)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.dtypeinfo = this;
            }
            if (id == Id.TypeInfo_Class)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.typeinfoclass = this;
            }
            if (id == Id.TypeInfo_Interface)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.typeinfointerface = this;
            }
            if (id == Id.TypeInfo_Struct)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.typeinfostruct = this;
            }
            if (id == Id.TypeInfo_Pointer)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.typeinfopointer = this;
            }
            if (id == Id.TypeInfo_Array)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.typeinfoarray = this;
            }
            if (id == Id.TypeInfo_StaticArray)
            {
                //if (!inObject)
                //    Type.typeinfostaticarray.classError("%s `%s` %s", msg);
                Type.typeinfostaticarray = this;
            }
            if (id == Id.TypeInfo_AssociativeArray)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.typeinfoassociativearray = this;
            }
            if (id == Id.TypeInfo_Enum)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.typeinfoenum = this;
            }
            if (id == Id.TypeInfo_Function)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.typeinfofunction = this;
            }
            if (id == Id.TypeInfo_Delegate)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.typeinfodelegate = this;
            }
            if (id == Id.TypeInfo_Tuple)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.typeinfotypelist = this;
            }
            if (id == Id.TypeInfo_Const)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.typeinfoconst = this;
            }
            if (id == Id.TypeInfo_Invariant)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.typeinfoinvariant = this;
            }
            if (id == Id.TypeInfo_Shared)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.typeinfoshared = this;
            }
            if (id == Id.TypeInfo_Wild)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.typeinfowild = this;
            }
            if (id == Id.TypeInfo_Vector)
            {
                if (!inObject)
                    classError("%s `%s` %s", msg.ptr);
                Type.typeinfovector = this;
            }
        }

        if (id == Id.Object)
        {
            if (!inObject)
                classError("%s `%s` %s", msg.ptr);
            object = this;
        }

        if (id == Id.Throwable)
        {
            if (!inObject)
                classError("%s `%s` %s", msg.ptr);
            throwable = this;
        }
        if (id == Id.Exception)
        {
            if (!inObject)
                classError("%s `%s` %s", msg.ptr);
            exception = this;
        }
        if (id == Id.Error)
        {
            if (!inObject)
                classError("%s `%s` %s", msg.ptr);
            errorException = this;
        }
        if (id == Id.cpp_type_info_ptr)
        {
            if (!inObject)
                classError("%s `%s` %s", msg.ptr);
            cpp_type_info_ptr = this;
        }

        baseok = Baseok.none;
    }

    extern (D) final void classError(const(char)* fmt, const(char)* arg)
    {
        .error(loc, fmt, kind, toPrettyChars, arg);
    }

    static ClassDeclaration create(Loc loc, Identifier id, BaseClasses* baseclasses, Dsymbols* members, bool inObject)
    {
        return new ClassDeclaration(loc, id, baseclasses, members, inObject);
    }

    override const(char)* toPrettyChars(bool qualifyTypes = false)
    {
        if (objc.isMeta)
            return .objc.toPrettyChars(this, qualifyTypes);

        return super.toPrettyChars(qualifyTypes);
    }

    override ClassDeclaration syntaxCopy(Dsymbol s)
    {
        //printf("ClassDeclaration.syntaxCopy('%s')\n", toChars());
        ClassDeclaration cd =
            s ? cast(ClassDeclaration)s
              : new ClassDeclaration(loc, ident, null, null, false);

        cd.storage_class |= storage_class;

        cd.baseclasses.setDim(this.baseclasses.length);
        for (size_t i = 0; i < cd.baseclasses.length; i++)
        {
            BaseClass* b = (*this.baseclasses)[i];
            auto b2 = new BaseClass(b.type.syntaxCopy());
            (*cd.baseclasses)[i] = b2;
        }

        ScopeDsymbol.syntaxCopy(cd);
        return cd;
    }

    override Scope* newScope(Scope* sc)
    {
        auto sc2 = super.newScope(sc);
        if (isCOMclass())
        {
            /* This enables us to use COM objects under Linux and
             * work with things like XPCOM
             */
            sc2.linkage = target.systemLinkage();
        }
        return sc2;
    }

    /*********************************************
     * Determine if 'this' is a base class of cd.
     * This is used to detect circular inheritance only.
     */
    extern (D) final bool isBaseOf2(ClassDeclaration cd) pure nothrow @nogc
    {
        if (!cd)
            return false;
        //printf("ClassDeclaration.isBaseOf2(this = '%s', cd = '%s')\n", toChars(), cd.toChars());
        for (size_t i = 0; i < cd.baseclasses.length; i++)
        {
            BaseClass* b = (*cd.baseclasses)[i];
            if (b.sym == this || isBaseOf2(b.sym))
                return true;
        }
        return false;
    }

    enum OFFSET_RUNTIME = 0x76543210;
    enum OFFSET_FWDREF = 0x76543211;

    /*******************************************
     * Determine if 'this' is a base class of cd.
     */
    bool isBaseOf(ClassDeclaration cd, int* poffset) pure nothrow @nogc
    {
        //printf("ClassDeclaration.isBaseOf(this = '%s', cd = '%s')\n", toChars(), cd.toChars());
        if (poffset)
            *poffset = 0;
        while (cd)
        {
            assert(cd.baseClass || cd.semanticRun >= PASS.semanticdone || cd.isInterfaceDeclaration());
            if (this == cd.baseClass)
                return true;

            cd = cd.baseClass;
        }
        return false;
    }

    /*********************************************
     * Determine if 'this' has complete base class information.
     * This is used to detect forward references in covariant overloads.
     */
    final bool isBaseInfoComplete() const
    {
        return baseok >= Baseok.done;
    }

    /************************************
     * Search base classes in depth-first, left-to-right order for
     * a class or interface named 'ident'.
     * Stops at first found. Does not look for additional matches.
     * Params:
     *  ident = identifier to search for
     * Returns:
     *  ClassDeclaration if found, null if not
     */
    extern (D) final ClassDeclaration searchBase(Identifier ident)
    {
        foreach (b; *baseclasses)
        {
            auto cdb = b.type.isClassHandle();
            if (!cdb) // https://issues.dlang.org/show_bug.cgi?id=10616
                return null;
            if (cdb.ident.equals(ident))
                return cdb;
            if (auto result = cdb.searchBase(ident))
                return result;
        }
        return null;
    }

    /**************
     * Returns: true if there's a __monitor field
     */
    final bool hasMonitor()
    {
        return classKind == ClassKind.d;
    }

    /****************
     * Find virtual function matching identifier and type.
     * Used to build virtual function tables for interface implementations.
     * Params:
     *  ident = function's identifier
     *  tf = function's type
     * Returns:
     *  function symbol if found, null if not
     * Errors:
     *  prints error message if more than one match
     */
    extern (D) final FuncDeclaration findFunc(Identifier ident, TypeFunction tf)
    {
        //printf("ClassDeclaration.findFunc(%s, %s) %s\n", ident.toChars(), tf.toChars(), toChars());
        FuncDeclaration fdmatch = null;
        FuncDeclaration fdambig = null;

        void updateBestMatch(FuncDeclaration fd)
        {
            fdmatch = fd;
            fdambig = null;
            //printf("Lfd fdmatch = %s %s [%s]\n", fdmatch.toChars(), fdmatch.type.toChars(), fdmatch.loc.toChars());
        }

        void searchVtbl(ref Dsymbols vtbl)
        {
            import dmd.typesem : covariant;
            bool seenInterfaceVirtual;
            foreach (s; vtbl)
            {
                auto fd = s.isFuncDeclaration();
                if (!fd)
                    continue;

                // the first entry might be a ClassInfo
                //printf("\t[%d] = %s\n", i, fd.toChars());
                if (ident != fd.ident || fd.type.covariant(tf) != Covariant.yes)
                {
                    //printf("\t\t%d\n", fd.type.covariant(tf));
                    continue;
                }

                //printf("fd.parent.isClassDeclaration() = %p\n", fd.parent.isClassDeclaration());
                if (!fdmatch)
                {
                    updateBestMatch(fd);
                    continue;
                }
                if (fd == fdmatch)
                    continue;

                /* Functions overriding interface functions for extern(C++) with VC++
                 * are not in the normal vtbl, but in vtblFinal. If the implementation
                 * is again overridden in a child class, both would be found here.
                 * The function in the child class should override the function
                 * in the base class, which is done here, because searchVtbl is first
                 * called for the child class. Checking seenInterfaceVirtual makes
                 * sure, that the compared functions are not in the same vtbl.
                 */
                if (fd.interfaceVirtual &&
                    fd.interfaceVirtual is fdmatch.interfaceVirtual &&
                    !seenInterfaceVirtual &&
                    fdmatch.type.covariant(fd.type) == Covariant.yes)
                {
                    seenInterfaceVirtual = true;
                    continue;
                }

                {
                // Function type matching: exact > covariant
                MATCH m1 = tf.equals(fd.type) ? MATCH.exact : MATCH.nomatch;
                MATCH m2 = tf.equals(fdmatch.type) ? MATCH.exact : MATCH.nomatch;
                if (m1 > m2)
                {
                    updateBestMatch(fd);
                    continue;
                }
                else if (m1 < m2)
                    continue;
                }
                {
                MATCH m1 = (tf.mod == fd.type.mod) ? MATCH.exact : MATCH.nomatch;
                MATCH m2 = (tf.mod == fdmatch.type.mod) ? MATCH.exact : MATCH.nomatch;
                if (m1 > m2)
                {
                    updateBestMatch(fd);
                    continue;
                }
                else if (m1 < m2)
                    continue;
                }
                {
                // The way of definition: non-mixin > mixin
                MATCH m1 = fd.parent.isClassDeclaration() ? MATCH.exact : MATCH.nomatch;
                MATCH m2 = fdmatch.parent.isClassDeclaration() ? MATCH.exact : MATCH.nomatch;
                if (m1 > m2)
                {
                    updateBestMatch(fd);
                    continue;
                }
                else if (m1 < m2)
                    continue;
                }

                fdambig = fd;
                //printf("Lambig fdambig = %s %s [%s]\n", fdambig.toChars(), fdambig.type.toChars(), fdambig.loc.toChars());
            }
        }

        searchVtbl(vtbl);
        for (auto cd = this; cd; cd = cd.baseClass)
        {
            searchVtbl(cd.vtblFinal);
        }

        if (fdambig)
            classError("%s `%s` ambiguous virtual function `%s`", fdambig.toChars());

        return fdmatch;
    }

    /****************************************
     */
    final bool isCOMclass() const
    {
        return com;
    }

    bool isCOMinterface() const
    {
        return false;
    }

    final bool isCPPclass() const
    {
        return classKind == ClassKind.cpp;
    }

    bool isCPPinterface() const
    {
        return false;
    }

    /****************************************
     * Determine if slot 0 of the vtbl[] is reserved for something else.
     * For class objects, yes, this is where the classinfo ptr goes.
     * For COM interfaces, no.
     * For non-COM interfaces, yes, this is where the Interface ptr goes.
     * Returns:
     *      0       vtbl[0] is first virtual function pointer
     *      1       vtbl[0] is classinfo/interfaceinfo pointer
     */
    int vtblOffset() const
    {
        return classKind == ClassKind.cpp ? 0 : 1;
    }

    /****************************************
     */
    override const(char)* kind() const
    {
        return "class";
    }

    /****************************************
     */
    override final void addObjcSymbols(ClassDeclarations* classes, ClassDeclarations* categories)
    {
        .objc.addSymbols(this, classes, categories);
    }

    // Back end
    Dsymbol vtblsym;

    extern (D) final bool isErrorException()
    {
        return errorException && (this == errorException || errorException.isBaseOf(this, null));
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class InterfaceDeclaration : ClassDeclaration
{
    extern (D) this(Loc loc, Identifier id, BaseClasses* baseclasses)
    {
        super(loc, id, baseclasses, null, false);
        this.dsym = DSYM.interfaceDeclaration;
        if (id == Id.IUnknown) // IUnknown is the root of all COM interfaces
        {
            com = true;
            classKind = ClassKind.cpp; // IUnknown is also a C++ interface
        }
    }

    override InterfaceDeclaration syntaxCopy(Dsymbol s)
    {
        InterfaceDeclaration id =
            s ? cast(InterfaceDeclaration)s
              : new InterfaceDeclaration(loc, ident, null);
        ClassDeclaration.syntaxCopy(id);
        return id;
    }


    override Scope* newScope(Scope* sc)
    {
        auto sc2 = super.newScope(sc);
        if (com)
            sc2.linkage = LINK.windows;
        else if (classKind == ClassKind.cpp)
            sc2.linkage = LINK.cpp;
        else if (classKind == ClassKind.objc)
            sc2.linkage = LINK.objc;
        return sc2;
    }

    /*******************************************
     * Determine if 'this' is a base class of cd.
     * (Actually, if it is an interface supported by cd)
     * Output:
     *      *poffset        offset to start of class
     *                      OFFSET_RUNTIME  must determine offset at runtime
     * Returns:
     *      false   not a base
     *      true    is a base
     */
    override bool isBaseOf(ClassDeclaration cd, int* poffset) pure nothrow @nogc
    {
        //printf("%s.InterfaceDeclaration.isBaseOf(cd = '%s')\n", toChars(), cd.toChars());
        assert(!baseClass);
        foreach (b; cd.interfaces)
        {
            //printf("\tX base %s\n", b.sym.toChars());
            if (this == b.sym)
            {
                //printf("\tfound at offset %d\n", b.offset);
                if (poffset)
                {
                    // don't return incorrect offsets
                    // https://issues.dlang.org/show_bug.cgi?id=16980
                    *poffset = cd.sizeok == Sizeok.done ? b.offset : OFFSET_FWDREF;
                }
                // printf("\tfound at offset %d\n", b.offset);
                return true;
            }
            if (baseClassImplementsInterface(this, b, poffset))
                return true;
        }
        if (cd.baseClass && isBaseOf(cd.baseClass, poffset))
            return true;

        if (poffset)
            *poffset = 0;
        return false;
    }

    /*******************************************
     */
    override const(char)* kind() const
    {
        return "interface";
    }

    /****************************************
     * Determine if slot 0 of the vtbl[] is reserved for something else.
     * For class objects, yes, this is where the ClassInfo ptr goes.
     * For COM interfaces, no.
     * For non-COM interfaces, yes, this is where the Interface ptr goes.
     */
    override int vtblOffset() const
    {
        if (isCOMinterface() || isCPPinterface())
            return 0;
        return 1;
    }

    override bool isCPPinterface() const
    {
        return classKind == ClassKind.cpp;
    }

    override bool isCOMinterface() const
    {
        return com;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/**
 * Returns whether `bc` implements `id`, including indirectly (`bc` implements an interfaces
 * that inherits from `id`)
 *
 * Params:
 *    id = the interface
 *    bc = the base class
 *    poffset = out parameter, offset of the interface in an object
 *
 * Returns:
 *    true if the `bc` implements `id`, false otherwise
 **/
private bool baseClassImplementsInterface(InterfaceDeclaration id, BaseClass* bc, int* poffset) pure nothrow @nogc @safe
{
    //printf("%s.InterfaceDeclaration.isBaseOf(bc = '%s')\n", id.toChars(), bc.sym.toChars());
    for (size_t j = 0; j < bc.baseInterfaces.length; j++)
    {
        BaseClass* b = &bc.baseInterfaces[j];
        //printf("\tY base %s\n", b.sym.toChars());
        if (id == b.sym)
        {
            //printf("\tfound at offset %d\n", b.offset);
            if (poffset)
            {
                *poffset = b.offset;
            }
            return true;
        }
        if (baseClassImplementsInterface(id, b, poffset))
        {
            return true;
        }
    }

    if (poffset)
        *poffset = 0;
    return false;
}
