// https://issues.dlang.org/show_bug.cgi?id=9701
// REQUIRED_ARGS: -verrors=simple
/*
TEST_OUTPUT:
---
compilable/test9701.d(69): Deprecation: enum member `test9701.Enum.value7` is deprecated
compilable/test9701.d(69): Deprecation: enum member `test9701.Enum.value8` is deprecated - message
---
*/

template AliasSeq(TList...)
{
    alias AliasSeq = TList;
}

enum
{
    uda4,
    uda5,
    uda6,
    uda8,
    uda9
}

enum Enum
{
    value0,
    @("uda1") value1,
    @("uda2", "uda3", 42) value2,
    @uda4 value3,
    @uda5 @uda6 value4,
    @("uda7") @uda8 value5,
    @uda9 @("uda10") value6,
    deprecated value7,
    deprecated("message") value8,
}

@("uda0")
enum
{
    value0,
    @("uda1") value1,
    @("uda2") @("uda3") value2,
    @uda4 value3,
    @uda5 @uda6 value4,
    @("uda7") @uda8 value5,
    @uda9 @("uda10") value6
}

static assert(__traits(getAttributes, Enum.value0).length == 0);
static assert(__traits(getAttributes, Enum.value1) == AliasSeq!("uda1"));
static assert(__traits(getAttributes, Enum.value2) == AliasSeq!("uda2", "uda3", 42));
static assert(__traits(getAttributes, Enum.value3) == AliasSeq!(uda4));
static assert(__traits(getAttributes, Enum.value4) == AliasSeq!(uda5, uda6));
static assert(__traits(getAttributes, Enum.value5) == AliasSeq!("uda7", uda8));
static assert(__traits(getAttributes, Enum.value6) == AliasSeq!(uda9, "uda10"));
static assert(__traits(isDeprecated, Enum.value7));
static assert(__traits(isDeprecated, Enum.value8));

static assert(__traits(getAttributes, value0) == AliasSeq!("uda0"));
static assert(__traits(getAttributes, value1) == AliasSeq!("uda0", "uda1"));
static assert(__traits(getAttributes, value2) == AliasSeq!("uda0", "uda2", "uda3"));
static assert(__traits(getAttributes, value3) == AliasSeq!("uda0", uda4));
static assert(__traits(getAttributes, value4) == AliasSeq!("uda0", uda5, uda6));
static assert(__traits(getAttributes, value5) == AliasSeq!("uda0", "uda7", uda8));
static assert(__traits(getAttributes, value6) == AliasSeq!("uda0", uda9, "uda10"));

// Test that messages are correctly displayed
static assert(Enum.value7 != Enum.value8);
