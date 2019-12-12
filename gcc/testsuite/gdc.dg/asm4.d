// https://issues.dlang.org/show_bug.cgi?id=12979
// { dg-do compile }
// { dg-options "-Wall -Wdeprecated -Werror" }
module asm4;

void test1()
{
    asm pure nothrow @nogc @trusted {}
    asm @safe {}
}

void test2() pure nothrow @nogc @safe
{
    asm pure nothrow @nogc @trusted {}
}

void test3()()
{
    asm pure nothrow @nogc @trusted {}
}

static assert(__traits(compiles, () pure nothrow @nogc @safe => test3()));

void test4()()
{
    asm {}
}

// wait for deprecation of asm pure inference
// static assert(!__traits(compiles, () pure => test4()));
static assert(!__traits(compiles, () nothrow => test4()));
// wait for deprecation of asm @nogc inference
// static assert(!__traits(compiles, () @nogc => test4()));
static assert(!__traits(compiles, () @safe => test4()));

@safe
void test5()
{
    static assert(!__traits(compiles, { asm { ""; } }() ));
}
