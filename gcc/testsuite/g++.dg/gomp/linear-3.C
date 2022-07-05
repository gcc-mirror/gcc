// { dg-do compile { target c++11 } }
// { dg-options "-fopenmp" }

int i;

#pragma omp declare simd linear (x : ref, step (1)) linear (y : step (2), uval)
int bar (int &x, int &y, int z);
#pragma omp declare simd linear (x : step (1), uval)
int baz (int &x, int y, int z);
#pragma omp declare simd linear (ref (x) : ref) uniform (ref)
int qux (int &x, int ref);
#pragma omp declare simd linear (x : ref, step (ref)) uniform (ref)
int corge (int &x, int ref);
#pragma omp declare simd linear (x : ref)
int grault (int &x);
#pragma omp declare simd linear (x : uval)
int waldo (int &x);
constexpr int step (int x) { return x; }

void
foo (int &x, int &y)
{
  #pragma omp simd linear (x: step (y + 1))
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp simd linear (x: val, step (y + 1))
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp parallel for linear (x: step (y + 1))
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp parallel for linear (x: step (y + 1), val)
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp parallel for simd linear (x: step (y + 1))
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp parallel for simd linear (x: val, step (y + 1))
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp parallel for simd linear (x: step (1) + 0)
  for (i = 0; i < 10; i++)
    x += step (1) + 0;
  {
    constexpr int ref = 1;
    constexpr int uval = 2;
    #pragma omp parallel for simd linear (x: ref + 0)
    for (i = 0; i < 10; i++)
      x += ref + 0;
    #pragma omp parallel for simd linear (x: uval * 1)
    for (i = 0; i < 10; i++)
      x += uval;
  }
}
