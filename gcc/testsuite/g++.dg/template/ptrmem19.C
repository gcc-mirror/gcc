// PR c++/40780
// { dg-do compile }

template <class T1, typename T2, typename T3>
struct A
{
  typedef T2 (T1::*m) (T3);
  A (m) {}
};
struct B;
struct C
{
  void foo (B *);
};
typedef A <C, void, B *> D;
typedef void (C::*E) (B *);
struct F;
typedef void (C::*G) (F);
D d ((E) (G) & C::foo);
