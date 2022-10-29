/* REQUIRED_ARGS: -preview=dip1000
 * TEST_OUTPUT:
---
fail_compilation/test19097.d(44): Error: scope variable `s` may not be returned
fail_compilation/test19097.d(48): Error: scope variable `s1` may not be returned
fail_compilation/test19097.d(77): Error: scope variable `z` assigned to `ref` variable `refPtr` with longer lifetime
fail_compilation/test19097.d(108): Error: scope variable `s4` may not be returned
fail_compilation/test19097.d(126): Error: scope variable `s5c` may not be returned
fail_compilation/test19097.d(130): Error: scope variable `s5m` may not be returned
fail_compilation/test19097.d(147): Error: scope variable `s6c` may not be returned
fail_compilation/test19097.d(151): Error: scope variable `s6m` may not be returned
---
 */

// Test extended return-scope / return-ref semantics, e.g. assigning to `this` or the first parameter

// https://issues.dlang.org/show_bug.cgi?id=19097

@safe:

void betty(ref scope int* r, return scope int* p)
{
    r = p;
}

void freddy(out scope int* r, return scope int* p)
{
    r = p;
}

struct S
{
    int* a;
    this(return scope int* b) scope { a = b; }

    int* c;
    void mem(return scope int* d) scope { c = d; }
}

S thorin()
{
    int i;
    S s = S(&i); // should infer scope for s
    return s;    // so this should error

    S s1;
    s1.mem(&i);
    return s1;
}

/************************/

struct S2(T)
{
    int* p;

    void silent(lazy void dg);

    void foo()
    {
        char[] name;
        silent(name = parseType());
    }

    char[] parseType(char[] name = null);
}

S2!int s2;

/************************/
struct S3
{
    int* ptr;
    void assign(ref int* refPtr, return scope int* z) scope @safe
    {
        this.ptr = z; // allowed, first ref
        refPtr = z; // should not be allowed
    }
}

int* escape() @safe
{
    int local;

    S3 escapeThis;
    int* escapeRef;

    escapeThis.assign(escapeRef, &local);

    return escapeRef;
}

/************************/
// https://issues.dlang.org/show_bug.cgi?id=22837
struct S4
{
    int* p;
    this(int dummy, return scope int* p) @safe
    {
        this.p = p;
    }
}

int* escape2()
{
    int x;
    auto s4 = S4(0, &x);
    return s4.p;
}

/************************/
// https://issues.dlang.org/show_bug.cgi?id=22801
struct S5
{
    int* a;
    this(return ref int b) { a = &b; }

    int* c;
    void mem(return ref int d) scope { c = &d; }
}

S5 frerin()
{
    int i;
    S5 s5c = S5(i); // should infer scope for s
    return s5c;    // so this should error

    S5 s5m;
    s5m.mem(i);
    return s5m;
}


struct S6
{
    int** a;
    this(return ref int* b) { a = &b; }

    int** c;
    void mem(return ref int* d) scope { c = &d; }
}

S6 dis()
{
    int* i = null;
    S6 s6c = S6(i); // should infer scope for s
    return s6c;    // so this should error

    S6 s6m;
    s6m.mem(i);
    return s6m;
}
