// { dg-do compile }
// { dg-options "-O" }
// { dg-final { scan-assembler "xyzzy" { xfail *-*-* } } }

struct S { S(); virtual void xyzzy(); };
inline void foo(S *s) { s->xyzzy(); }
void bar() { S s; foo(&s); }
