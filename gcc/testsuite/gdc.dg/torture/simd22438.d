// https://issues.dlang.org/show_bug.cgi?id=22438
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

struct T22438 { int x; double d; }

T22438 foo22438(int x, double d) { return T22438(x, d); }

struct S22438 { T22438 t; string r; }

void main()
{
    S22438 s = S22438(foo22438(10, 3.14), "str");
    assert(s.t.x == 10);
    assert(s.t.d == 3.14);
    assert(s.r == "str");
}
