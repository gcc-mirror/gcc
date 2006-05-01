/* PR c/27358 */
/* { dg-do compile } */

void foo(error i)		/* { dg-error "" } */
{
#pragma omp parallel
  i = 0;
}
