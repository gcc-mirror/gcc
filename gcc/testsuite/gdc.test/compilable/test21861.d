// https://issues.dlang.org/show_bug.cgi?id=21861

int f() {
    @("S")
    struct S {}

    @("U")
    union U {}

    @("C")
    class C {}

    // OK <- CTFE fails on this:
    @("E")
    enum E { X }

    // OK <- CTFE fails on this:
    @("f")
    int f(int x) { return x + 2; }

    // OK <- CTFE fails on this:
    @(&f) int a;
    @(1) @(2) int b = 4, c;
    @(3) extern(C) int d = 3;

    enum uda1 = __traits(getAttributes, a);
    enum uda2 = __traits(getAttributes, b);
    enum uda3 = __traits(getAttributes, c);

    // These are to trigger a compiler assert if parser is updated in the future
    static assert(!__traits(compiles, mixin("{ @(1) { int x; int y; } }")));
    static assert(!__traits(compiles, mixin("{ @(1): int x; int y; }")));

    //        3+2         1         2       4      1      3
    return uda1[0](3) + uda2[0] + uda2[1] + b + uda3[0] + d;
}

static assert(f() == 16);
