// PR sanitizer/83987
// { dg-do compile { target fopenmp } }
// { dg-options "-fopenmp -fsanitize=vptr" }

struct A
{
  int i;
};

struct B : virtual A
{
  void foo();
};

void B::foo()
{
#pragma omp parallel
  {
  #pragma omp sections lastprivate (i)
    {
      i = 0;
    }
  }
}
