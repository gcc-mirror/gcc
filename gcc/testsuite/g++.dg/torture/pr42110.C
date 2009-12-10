/* { dg-do compile } */
bool foo();

struct A
{
  A* fooA() { if (foo()) foo(); return this; }

  virtual void barA(char);
};

template<int> struct B
{
  A *p, *q;

  void fooB(char c) { p->fooA()->barA(c); }
};

template<int N> inline void bar(B<N> b) { b.fooB(0); }

extern template void bar(B<0>);

void (*f)(B<0>) = bar;

void baz()
{
  B<0>().fooB(0);
}
