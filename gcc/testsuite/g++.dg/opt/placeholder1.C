// PR rtl-optimization/15159
// { dg-options "-O2" }
struct S { S (); };
struct P { P (S *); };
void foo (const P &);
void bar ()
{
  P p = new S;
  foo (p);
}
