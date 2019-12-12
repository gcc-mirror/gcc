// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-additional-options "-g" }

int main()
{
  int n = 0;
#pragma omp parallel
#pragma omp master
#pragma omp task shared (n)
  for (int i = [](){ return 3; }(); i < 10; ++i)
    n = i;
#pragma omp taskwait
  if (n != 7)
    __builtin_abort ();
}
