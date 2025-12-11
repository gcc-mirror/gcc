/* PR c++/24513 */
/* { dg-do compile } */
// { dg-additional-options "-Wno-deprecated-openmp" }
struct S
{
  void foo (int *p)
  {
#pragma omp parallel for
    for (int i = 0; i < 1000; ++i)
      p[i]=0;
  }
  void bar ()
  {
#pragma omp master
    j = 2;
  }
  int j;
};
