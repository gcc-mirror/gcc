/* PR middle-end/45423 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -Wno-deprecated" } */
/* { dg-skip-if "invalid in C++17" { c++17 } } */

#ifdef __cplusplus
bool *baz ();
#else
_Bool *baz ();
#endif
int *bar ();

int
foo (void)
{
  #pragma omp barrier
  #pragma omp atomic
    (*bar ())++;
  #pragma omp barrier
  #pragma omp atomic
    ++(*bar ());
  #pragma omp barrier
  #pragma omp atomic
    (*bar ())--;
  #pragma omp barrier
  #pragma omp atomic
    --(*bar ());
  #pragma omp barrier
  #pragma omp atomic
    (*baz ())++;
  #pragma omp barrier
  #pragma omp atomic
    ++(*baz ());
#ifndef __cplusplus
  #pragma omp barrier
  #pragma omp atomic
    (*baz ())--;
  #pragma omp barrier
  #pragma omp atomic
    --(*baz ());
  #pragma omp barrier
#endif
  return 0;
}
