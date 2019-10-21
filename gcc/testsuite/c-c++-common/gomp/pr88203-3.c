/* PR c++/88203 */
/* { dg-do compile } */
/* { dg-additional-options "-std=c99" { target c } } */
/* { dg-additional-options "-std=c++11" { target c++ } } */

void foo (const char *);
#pragma omp declare target to (foo)

void
f1 (void)
{
  #pragma omp parallel for lastprivate (__func__)	/* { dg-error "'__func__' is predetermined 'shared' for 'lastprivate'" } */
  for (int i = 0; i < 2; i++)
    foo (__func__);
  #pragma omp parallel private (__func__)		/* { dg-error "'__func__' is predetermined 'shared' for 'private'" } */
  foo (__func__);
}

void
f2 (void)
{
  foo (__func__);
  #pragma omp parallel default(none) private (__func__)		/* { dg-error "'__func__' is predetermined 'shared' for 'private'" } */
  foo (__func__);
  #pragma omp parallel for default(none) lastprivate (__func__)	/* { dg-error "'__func__' is predetermined 'shared' for 'lastprivate'" } */
  for (int i = 0; i < 2; i++)
    foo (__func__);
}
