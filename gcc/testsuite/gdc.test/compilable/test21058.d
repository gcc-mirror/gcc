struct A {
    int foo(string s) { return 0; }
}

static assert(__traits(getOverloads, A.init, "foo")[0]("hi") == 0);
static assert(__traits(getOverloads, A.init, "foo", true)[0]("hi") == 0);

struct B {
    int foo()(int i) { return 1; }
    int foo(string s) { return 0; }
}

alias a = __traits(getOverloads, B.init, "foo", true);

static assert(__traits(getOverloads, B.init, "foo")[0]("hi") == 0);
static assert(__traits(getOverloads, B.init, "foo", true)[0]("hi") == 0);

struct C {
    static int foo()(int i) { return 1; }
    int foo(string s) { return 0; }
}

static assert(__traits(getOverloads, C.init, "foo")[0]("hi") == 0);
static assert(__traits(getOverloads, C.init, "foo", true)[0]("hi") == 0);
static assert(__traits(getOverloads, C.init, "foo", true)[1](7) == 1);
