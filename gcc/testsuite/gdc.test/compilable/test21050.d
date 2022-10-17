/*
TEST_OUTPUT:
----
instantiated:long
instantiated:int
----
*/
struct S {
    static int foo(T)(int i) { pragma(msg, "instantiated:", T.stringof); return 0; }
    static int foo(T)(string s) { return 1; }
}

alias foo0 = __traits(getOverloads, S, "foo", true)[0];
alias bar0 = foo0!long; // prints "instantiated:long"
enum x = S.foo!long(0); // should not print "instantiated:long" again.
static assert(bar0(3) == 0);
alias bar0int = foo0!int; // prints "instantiated:int"
enum y = S.foo!int(0); // should not print "instantiated:int" again.
static assert(!__traits(compiles, bar0("hi")));

alias foo1 = __traits(getOverloads, S, "foo", true)[1];
alias bar1 = foo1!long;
static assert(bar1("hi") == 1);
static assert(!__traits(compiles, bar1(3)));
