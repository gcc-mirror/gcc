// PERMUTE_ARGS:

// MT!"y" is analyzed from the pragma inside MT!"x"
/*
TEST_OUTPUT:
---
CT y.offsetof = <
0 > y
CT x.offsetof = <
0 > x
---
*/

mixin template MT(string id)
{
    union
    {
        mixin("void* " ~ id ~ ";");
    }

    // Evaluating `typeof(this).init` completes data layout.
    pragma(msg,
        "CT " ~ id ~ ".offsetof = <\n",
        cast(int)typeof(this).init.i.offsetof, " > " ~ id);
}

struct S1
{
    int i;
    mixin MT!"x";
    mixin MT!"y";
}
struct S2
{
    int i;
    union { void* x; }
    union { void* y; }
}
struct S3
{
    int i;
    void* x;
    void* y;
}

void main()
{
    // S1, S2, and S3 should have exactly same data layout.
    static assert(S1.i.offsetof == S3.i.offsetof);
    static assert(S1.i.offsetof == S3.i.offsetof);
    static assert(S1.x.offsetof == S3.x.offsetof);
    static assert(S1.y.offsetof == S3.y.offsetof);
    static assert(S2.x.offsetof == S3.x.offsetof);
    static assert(S2.y.offsetof == S3.y.offsetof);
    static assert(S1.sizeof == S3.sizeof);
    static assert(S2.sizeof == S3.sizeof);

    S1 s = void;

    s.i = 1;
    assert(s.i == 1);
    s.x = null;
    assert(s.i == 1);
    s.y = null;
    assert(s.i == 1);

    char a, b;
    s.x = cast(void*)&a;
    assert(s.x is &a);
    assert(s.y is null);
    s.y = cast(void*)&b;
    assert(s.x is &a);
    assert(s.y is &b);
}
