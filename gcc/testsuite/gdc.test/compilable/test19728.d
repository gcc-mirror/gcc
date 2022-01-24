/*
TEST_OUTPUT:
---
tuple((A), (B))
tuple((A), (B), 0)
tuple((A), (B), (A))
tuple((A), (B), (A), (B))
tuple((A), (B), (A), (B))
tuple((A), (B), (A), (B), (A), (B), (A), (B))
tuple((Attr))
---
*/

// Issue 19728
enum A; enum B; enum Dummy = 0;

alias Seq(T...) = T;

@Seq!(A,B) struct Foo1 {}
@Seq!(A,B, Dummy) struct Foo2 {}
@Seq!(A,B,A) struct Foo3 {}
@Seq!(Seq!(A,B,A,B)) struct Foo4 {}
@Seq!(A,Seq!(B,A),B) struct Foo5 {}
@Seq!(A,Seq!(B,A),B) @Seq!(A,B,A,B) struct Foo6 {}

pragma(msg, __traits(getAttributes, Foo1));
pragma(msg, __traits(getAttributes, Foo2));
pragma(msg, __traits(getAttributes, Foo3));
pragma(msg, __traits(getAttributes, Foo4));
pragma(msg, __traits(getAttributes, Foo5));
pragma(msg, __traits(getAttributes, Foo6));

struct S(T...) {}
static assert(is(            S!(A,B) == S!(__traits(getAttributes, Foo1))));
static assert(is(      S!(A,B,Dummy) == S!(__traits(getAttributes, Foo2))));
static assert(is(          S!(A,B,A) == S!(__traits(getAttributes, Foo3))));
static assert(is(        S!(A,B,A,B) == S!(__traits(getAttributes, Foo4))));
static assert(is(        S!(A,B,A,B) == S!(__traits(getAttributes, Foo5))));
static assert(is(S!(A,B,A,B,A,B,A,B) == S!(__traits(getAttributes, Foo6))));

// Issue 20093
mixin template MakeProperty(Attributes...) {
    @(Attributes) void bug() {}
}

struct Attr { }

struct Test {
    mixin MakeProperty!(Attr);
}

pragma(msg, __traits(getAttributes, Test.bug));
