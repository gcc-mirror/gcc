// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-additional-options "-g" }

int main()
{
  int n = 0;
#pragma omp task shared(n)
#pragma omp target map(tofrom:n)
#pragma omp for reduction (+: n)
  for (int i = [](){ return 3; }(); i < 10; ++i)
    n++;
  if (n != 7)
    __builtin_abort ();
#pragma omp taskwait
  return 0;
}
