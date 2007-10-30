// PR c++/33616
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort ();

struct S {
  int c;
  S () : c (0) {}
  virtual void f1 () { c += 1; }
  virtual void f2 () { c += 16; }
};

struct T {
  S s;
};

typedef void (S::*Q) ();

template <Q P>
void test1 (T *t)
{
  (t->s.*P)();
}

template <Q P>
void test2 (T *t)
{
  S &s = t->s;
  (s.*P)();
}

int
main ()
{
  T t;
  test1 <&S::f1> (&t);
  if (t.s.c != 1)
    abort ();
  test1 <&S::f2> (&t);
  if (t.s.c != 17)
    abort ();
  test2 <&S::f1> (&t);
  if (t.s.c != 18)
    abort ();
  test2 <&S::f2> (&t);
  if (t.s.c != 34)
    abort ();
}
