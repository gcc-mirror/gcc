// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-additional-options "-g" }

int main()
{
  int n = 0;

#pragma omp parallel
#pragma omp master
#pragma omp parallel
#pragma omp master
#pragma omp parallel for reduction (+: n)
  for (int i = [](){ return 3; }(); i < 10; ++i)
    n++;

  return n;
}
