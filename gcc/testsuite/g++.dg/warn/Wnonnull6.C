/* PR c++/95984 - Internal compiler error: Error reporting routines re-entered
   in -Wnonnull on a variadic lambda
   PR c++/missing -Wnonnull passing nullptr to a nonnull variadic lambda
   { dg-do compile { target c++14 } }
   { dg-options "-Wall" } */

typedef int F (int);

F* pr95984 ()
{
  // This also triggered the ICE.
  return [](auto...) { return 0; };     // { dg-bogus "\\\[-Wnonnull" }
}


__attribute__ ((nonnull)) void f (int, ...);
void ff ()
{
  f (1, nullptr);                       // { dg-warning "\\\[-Wnonnull" }
}

template <class T> void g (T t)
{
  t (1, nullptr);                       // { dg-warning "\\\[-Wnonnull" }
}

void gg (void)
{
  g ([](int, auto...) __attribute__ ((nonnull)) { });
}

template <class T> __attribute__ ((nonnull)) void h (T);

void hh ()
{
  h (nullptr);                          //  { dg-warning "\\\[-Wnonnull" }
}
