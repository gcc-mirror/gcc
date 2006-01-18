/* { dg-do compile } */

int a[4];
int *p;
struct S { int x; int y[4]; } s;
int *bar(void);

void f1(void)
{
  #pragma omp atomic
    a[4] += 1;
  #pragma omp atomic
    *p += 1;
  #pragma omp atomic
    s.x += 1;
  #pragma omp atomic
    s.y[*p] += 1;
  #pragma omp atomic
    s.y[*p] *= 42;
  #pragma omp atomic
    *bar() += 1;
  #pragma omp atomic
    *bar() *= 42;
}
