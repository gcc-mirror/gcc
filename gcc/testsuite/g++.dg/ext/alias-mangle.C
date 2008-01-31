// PR c++/34936
// { dg-do compile }
/* { dg-final { scan-assembler "_ZN1AIdEC1Ev" } } */
typedef double X __attribute((may_alias)) ;

template<typename> struct A
{
  A();
};

A<X> a;
