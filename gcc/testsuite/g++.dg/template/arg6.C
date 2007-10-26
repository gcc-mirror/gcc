// PR c++/33744
// { dg-do run }

template <bool B> struct A { bool b; A() : b(B) {}; };
A<bool(1)> a;
A<bool(1<2)> b;
A<(bool)(2>1)> c;
A<bool((2>1))> d;
A<bool(2>1)> e;

int
main ()
{
  return (a.b && b.b && c.b && d.b && e.b) ? 0 : 1;
}
