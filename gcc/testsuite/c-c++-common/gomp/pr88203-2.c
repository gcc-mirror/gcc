/* PR c++/88203 */
/* { dg-do compile } */
/* { dg-additional-options "-std=gnu99" { target c } } */
/* { dg-additional-options "-std=gnu++11" { target c++ } } */

void foo (const char *, const char *);
#pragma omp declare target to (foo)

void
f1 (void)
{
  #pragma omp parallel default(none)
  foo (__FUNCTION__, __PRETTY_FUNCTION__);
}

void
f2 (void)
{
  #pragma omp parallel default(none) shared(__FUNCTION__, __PRETTY_FUNCTION__)
  foo (__FUNCTION__, __PRETTY_FUNCTION__);
  #pragma omp parallel default(none) shared(__FUNCTION__) firstprivate(__PRETTY_FUNCTION__)
  foo (__FUNCTION__, __PRETTY_FUNCTION__);
}

void
f3 (void)
{
  #pragma omp parallel default(none) firstprivate(__FUNCTION__, __PRETTY_FUNCTION__)
  foo (__FUNCTION__, __PRETTY_FUNCTION__);
  #pragma omp parallel default(none) firstprivate(__FUNCTION__), shared(__PRETTY_FUNCTION__)
  foo (__FUNCTION__, __PRETTY_FUNCTION__);
}

void
f4 (void)
{
  foo (__FUNCTION__, __PRETTY_FUNCTION__);
  #pragma omp parallel default(none)
  foo (__FUNCTION__, __PRETTY_FUNCTION__);
}

void
f5 (void)
{
  foo (__FUNCTION__, __PRETTY_FUNCTION__);
  #pragma omp parallel default(none) shared(__FUNCTION__, __PRETTY_FUNCTION__)
  foo (__FUNCTION__, __PRETTY_FUNCTION__);
}

void
f6 (void)
{
  foo (__FUNCTION__, __PRETTY_FUNCTION__);
  #pragma omp parallel default(none) firstprivate(__FUNCTION__, __PRETTY_FUNCTION__)
  foo (__FUNCTION__, __PRETTY_FUNCTION__);
}

void
f7 (void)
{
  #pragma omp target map(to: __FUNCTION__, __PRETTY_FUNCTION__)
  foo (__FUNCTION__, __PRETTY_FUNCTION__);
  #pragma omp task depend(inout:__FUNCTION__, __PRETTY_FUNCTION__)
  foo (__FUNCTION__, __PRETTY_FUNCTION__);
}
