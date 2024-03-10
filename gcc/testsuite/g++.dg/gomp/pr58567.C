/* { dg-do compile } */

/* PR c++/58567 - was ICEing before */

template<typename T> void foo()
{
  #pragma omp parallel for
  for (typename T::X i = 0; i < 100; ++i)  /* { dg-error "'int' is not a class, struct, or union type|invalid type for iteration variable 'i'" } */
    ;
}

void bar()
{
  foo<int>();
}
