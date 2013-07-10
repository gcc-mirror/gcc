/* PR preprocessor/57824 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -fopenmp" { target c } } */
/* { dg-options "-std=c++11 -fopenmp" { target c++ } } */

void bar ();

void foo ()
{
#pragma omp parallel num_threads(sizeof R"(
abc
)")
  bar ();
}
