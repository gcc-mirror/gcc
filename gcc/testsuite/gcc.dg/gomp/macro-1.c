// { dg-do compile }

#define N 10

extern void bar(void);
void foo(void)
{
  #pragma omp parallel num_threads(N)
    bar();
}
