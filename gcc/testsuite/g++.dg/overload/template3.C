// PR c++/33962
// { dg-do compile }

template <class T> struct A;

template <class U> void foo (const U &x, ...);
template <class T> void foo (const A<T> &x, ...);

void bar (const A<int> &x, const char *y)
{
  foo (x, y);
}

/* { dg-final { scan-assembler "_Z3fooIiEvRK1AIT_Ez" } } */
/* { dg-final { scan-assembler-not "_Z3fooI1AIiEEvRKT_z" } } */
