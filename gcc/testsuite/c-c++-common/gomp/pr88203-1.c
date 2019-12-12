/* PR c++/88203 */
/* { dg-do compile } */
/* { dg-additional-options "-std=c99" { target c } } */
/* { dg-additional-options "-std=c++11" { target c++ } } */

void foo (const char *);
#pragma omp declare target to (foo)

void
f1 (void)
{
  #pragma omp parallel default(none)
  foo (__func__);
}

void
f2 (void)
{
  #pragma omp parallel default(none) shared(__func__)
  foo (__func__);
}

void
f3 (void)
{
  #pragma omp parallel default(none) firstprivate(__func__)
  foo (__func__);
}

void
f4 (void)
{
  foo (__func__);
  #pragma omp parallel default(none)
  foo (__func__);
}

void
f5 (void)
{
  foo (__func__);
  #pragma omp parallel default(none) shared(__func__)
  foo (__func__);
}

void
f6 (void)
{
  foo (__func__);
  #pragma omp parallel default(none) firstprivate(__func__)
  foo (__func__);
}

void
f7 (void)
{
  #pragma omp target map(to: __func__)
  foo (__func__);
  #pragma omp task depend(inout:__func__)
  foo (__func__);
}
