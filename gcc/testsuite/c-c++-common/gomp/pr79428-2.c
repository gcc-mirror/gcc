/* PR c/79428 */
/* { dg-options "-fopenmp" } */
void
foo ()
{
#pragma omp sections
#pragma omp section /* { dg-error "'#pragma omp section' may only be used in '#pragma omp sections' construct|not allowed|expected" } */
