// EXTRA_FILES: imports/test19344.d

// https://issues.dlang.org/show_bug.cgi?id=20835

template T(E) {
    alias T =  __traits(getAttributes, E.a);
}

void main()
{
    class C {}
    enum E {
        @C a
    }

    alias b = T!E;
}

// https://issues.dlang.org/show_bug.cgi?id=19344

import imports.test19344;

struct Struct {
    int value;
}

enum Enum {
    @Struct(42) first,
}

static assert(getUDAs!(Enum.first, Struct)[0] == Struct(42));
static assert(__traits(getAttributes, Enum.first)[0] == Struct(42));

// https://issues.dlang.org/show_bug.cgi?id=21122

void test21122()
{
    enum A;
    enum E { @A a }

    static assert(is(getAllUDAs!(E.a)[0] == A));
}

alias getAllUDAs(A...) = __traits(getAttributes, A);

// https://issues.dlang.org/show_bug.cgi?id=21352

@("aaa") enum Hoge {
        @("bbb") foo, // tuple("aaa", "bbb") -> should be only tuple("bbb")
        bar,  // tuple()
}
@("aaa") struct Fuga {
        @("bbb") int foo; // tuple("bbb")
        int bar; // tuple()
}
static assert([__traits(getAttributes, Hoge.foo)] == ["bbb"]); //NG -> fixed
static assert([__traits(getAttributes, Hoge.bar)] == []);
static assert([__traits(getAttributes, Fuga.foo)] == ["bbb"]);
static assert([__traits(getAttributes, Fuga.bar)] == []);
