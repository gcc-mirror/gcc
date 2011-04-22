// { dg-do compile }
// { dg-options "-O2" }
// { dg-final { scan-assembler-times "xyzzy" 2 } }

struct S { S(); virtual void xyzzy(); };
struct R { int a; S s; R(); };
S s;
R r;
inline void foo(S *p) { p->xyzzy(); }
void bar() {foo(&s);}
void bah() {foo(&r.s);}
