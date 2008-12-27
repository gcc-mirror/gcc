// PR middle-end/38633
// { dg-do compile }
// { dg-options "-fopenmp" }

void
foo ()
{
#pragma omp parallel
  {
    struct A { int i; } j;
    j.i = 6;
    j.i++;
  }
}
