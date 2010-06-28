// PR c++/44682
// { dg-do compile }
// { dg-options "-Wunused" }

struct S { virtual ~S () {} };
struct T { virtual ~T () {} };
struct U : S, T {};

void f (U &);

void
g (void *v)
{
  T *t = static_cast <T *> (v);
  U *u = static_cast <U *> (t);
  f (*u);
}
