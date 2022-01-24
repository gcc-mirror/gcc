
module traits_getFunctionAttributes;

void test_getFunctionAttributes()
{
    alias tuple(T...) = T;

    struct S
    {
        int noF() { return 0; }
        int constF() const { return 0; }
        int immutableF() immutable { return 0; }
        int inoutF() inout { return 0; }
        int sharedF() shared { return 0; }

        int x;
        ref int refF() return { return x; }
        int propertyF() @property { return 0; }
        int nothrowF() nothrow { return 0; }
        int nogcF() @nogc { return 0; }

        int systemF() @system { return 0; }
        int trustedF() @trusted { return 0; }
        int safeF() @safe { return 0; }

        int pureF() pure { return 0; }

        int liveF() @live { return 0; }
    }

    static assert(__traits(getFunctionAttributes, S.noF) == tuple!("@system"));
    static assert(__traits(getFunctionAttributes, typeof(S.noF)) == tuple!("@system"));

    static assert(__traits(getFunctionAttributes, S.constF) == tuple!("const", "@system"));
    static assert(__traits(getFunctionAttributes, typeof(S.constF)) == tuple!("const", "@system"));

    static assert(__traits(getFunctionAttributes, S.immutableF) == tuple!("immutable", "@system"));
    static assert(__traits(getFunctionAttributes, typeof(S.immutableF)) == tuple!("immutable", "@system"));

    static assert(__traits(getFunctionAttributes, S.inoutF) == tuple!("inout", "@system"));
    static assert(__traits(getFunctionAttributes, typeof(S.inoutF)) == tuple!("inout", "@system"));

    static assert(__traits(getFunctionAttributes, S.sharedF) == tuple!("shared", "@system"));
    static assert(__traits(getFunctionAttributes, typeof(S.sharedF)) == tuple!("shared", "@system"));

    static assert(__traits(getFunctionAttributes, S.refF) == tuple!("ref", "return", "@system"));
    static assert(__traits(getFunctionAttributes, typeof(S.refF)) == tuple!("ref", "return", "@system"));

    static assert(__traits(getFunctionAttributes, S.propertyF) == tuple!("@property", "@system"));
    static assert(__traits(getFunctionAttributes, typeof(&S.propertyF)) == tuple!("@property", "@system"));

    static assert(__traits(getFunctionAttributes, S.nothrowF) == tuple!("nothrow", "@system"));
    static assert(__traits(getFunctionAttributes, typeof(S.nothrowF)) == tuple!("nothrow", "@system"));

    static assert(__traits(getFunctionAttributes, S.nogcF) == tuple!("@nogc", "@system"));
    static assert(__traits(getFunctionAttributes, typeof(S.nogcF)) == tuple!("@nogc", "@system"));

    static assert(__traits(getFunctionAttributes, S.systemF) == tuple!("@system"));
    static assert(__traits(getFunctionAttributes, typeof(S.systemF)) == tuple!("@system"));

    static assert(__traits(getFunctionAttributes, S.trustedF) == tuple!("@trusted"));
    static assert(__traits(getFunctionAttributes, typeof(S.trustedF)) == tuple!("@trusted"));

    static assert(__traits(getFunctionAttributes, S.safeF) == tuple!("@safe"));
    static assert(__traits(getFunctionAttributes, typeof(S.safeF)) == tuple!("@safe"));

    static assert(__traits(getFunctionAttributes, S.pureF) == tuple!("pure", "@system"));
    static assert(__traits(getFunctionAttributes, typeof(S.pureF)) == tuple!("pure", "@system"));

    static assert(__traits(getFunctionAttributes, S.liveF) == tuple!("@live", "@system"));
    static assert(__traits(getFunctionAttributes, typeof(S.liveF)) == tuple!("@live", "@system"));

    int pure_nothrow() nothrow pure { return 0; }
    static ref int static_ref_property() @property { return *(new int); }
    ref int ref_property() @property { return *(new int); }
    void safe_nothrow() @safe nothrow { }
    void live_nothrow() nothrow @live { }

    static assert(__traits(getFunctionAttributes, pure_nothrow) == tuple!("pure", "nothrow", "@nogc", "@safe"));
    static assert(__traits(getFunctionAttributes, typeof(pure_nothrow)) == tuple!("pure", "nothrow", "@nogc", "@safe"));

    static assert(__traits(getFunctionAttributes, static_ref_property) == tuple!("pure", "nothrow", "@property", "ref", "@safe"));
    static assert(__traits(getFunctionAttributes, typeof(&static_ref_property)) == tuple!("pure", "nothrow", "@property", "ref", "@safe"));

    static assert(__traits(getFunctionAttributes, ref_property) == tuple!("pure", "nothrow", "@property", "ref", "@safe"));
    static assert(__traits(getFunctionAttributes, typeof(&ref_property)) == tuple!("pure", "nothrow", "@property", "ref", "@safe"));

    static assert(__traits(getFunctionAttributes, safe_nothrow) == tuple!("pure", "nothrow", "@nogc", "@safe"));
    static assert(__traits(getFunctionAttributes, typeof(safe_nothrow)) == tuple!("pure", "nothrow", "@nogc", "@safe"));

    static assert(__traits(getFunctionAttributes, live_nothrow) == tuple!("pure", "nothrow", "@nogc", "@live", "@safe"));
    static assert(__traits(getFunctionAttributes, typeof(live_nothrow)) == tuple!("pure", "nothrow", "@nogc", "@live", "@safe"));

    struct S2
    {
        int pure_const() const pure { return 0; }
        int pure_sharedconst() const shared pure { return 0; }
    }

    static assert(__traits(getFunctionAttributes, S2.pure_const) == tuple!("const", "pure", "@system"));
    static assert(__traits(getFunctionAttributes, typeof(S2.pure_const)) == tuple!("const", "pure", "@system"));

    static assert(__traits(getFunctionAttributes, S2.pure_sharedconst) == tuple!("const", "shared", "pure", "@system"));
    static assert(__traits(getFunctionAttributes, typeof(S2.pure_sharedconst)) == tuple!("const", "shared", "pure", "@system"));

    static assert(__traits(getFunctionAttributes, (int a) { }) == tuple!("pure", "nothrow", "@nogc", "@safe"));
    static assert(__traits(getFunctionAttributes, typeof((int a) { })) == tuple!("pure", "nothrow", "@nogc", "@safe"));

    auto safeDel = delegate() @safe { };
    static assert(__traits(getFunctionAttributes, safeDel) == tuple!("pure", "nothrow", "@nogc", "@safe"));
    static assert(__traits(getFunctionAttributes, typeof(safeDel)) == tuple!("pure", "nothrow", "@nogc", "@safe"));

    auto trustedDel = delegate() @trusted { };
    static assert(__traits(getFunctionAttributes, trustedDel) == tuple!("pure", "nothrow", "@nogc", "@trusted"));
    static assert(__traits(getFunctionAttributes, typeof(trustedDel)) == tuple!("pure", "nothrow", "@nogc", "@trusted"));

    auto systemDel = delegate() @system { };
    static assert(__traits(getFunctionAttributes, systemDel) == tuple!("pure", "nothrow", "@nogc", "@system"));
    static assert(__traits(getFunctionAttributes, typeof(systemDel)) == tuple!("pure", "nothrow", "@nogc", "@system"));
}
