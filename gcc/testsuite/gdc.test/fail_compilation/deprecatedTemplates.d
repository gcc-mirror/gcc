/*
REQUIRED_ARGS: -de

TEST_OUTPUT:
----
fail_compilation/deprecatedTemplates.d(103): Deprecation: template `deprecatedTemplates.AliasSeq(V...)` is deprecated
fail_compilation/deprecatedTemplates.d(107): Deprecation: struct `deprecatedTemplates.S1(V...)` is deprecated
fail_compilation/deprecatedTemplates.d(115): Deprecation: template `deprecatedTemplates.C(V...)` is deprecated
----
*/
#line 100

deprecated alias AliasSeq(V...) = V;

alias x = AliasSeq!(1, 2, 3);

deprecated struct S1(V...) {}

alias T1 = S1!();

deprecated template C(V...)
{
    int i;
    int j;
}

alias D = C!();

/*
TEST_OUTPUT:
----
fail_compilation/deprecatedTemplates.d(202): Deprecation: template `deprecatedTemplates.AliasSeqMsg(V...)` is deprecated - Reason
----
*/
#line 200
deprecated("Reason") alias AliasSeqMsg(V...) = V;

alias xMsg = AliasSeqMsg!(1, 2, 3);

deprecated struct DS()
{
    S1!() s;
}

deprecated struct DS2()
{
    static struct DS3()
    {
        S1!() s;
    }

    static struct DS4
    {
        S1!() s;
    }
}

deprecated void foo()
{
    DS!() d1;
    DS2!().DS3!() d2;
    DS2!().DS4 d3;
}
