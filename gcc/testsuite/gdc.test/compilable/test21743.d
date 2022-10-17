// https://issues.dlang.org/show_bug.cgi?id=21743

struct A
{
    int foo(int a) { return a; }
    string foo()(string b) { return b; }
}

alias ov = __traits(getOverloads, A.init, "foo", true);

// member function works
static assert(ov[0](1) == 1);

// member template used to fail with the gagged error:
// 'need this for foo of type pure nothrow @nogc @safe string(string b)'
static assert(ov[1]("a") == "a");
