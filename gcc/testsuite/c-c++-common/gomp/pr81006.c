/* PR c/81006 */
/* { dg-do compile } */

int a[] = {};

void foo()
{
  #pragma omp task depend(out: a[:])	/* { dg-error "zero length array section in .depend. clause" } */
    {}
}
