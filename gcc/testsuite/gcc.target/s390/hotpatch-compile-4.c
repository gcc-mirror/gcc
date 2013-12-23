/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -mhotpatch=-1" } */

int main (void)
{
  return 0;
}

/* { dg-excess-errors "argument to '-mhotpatch=' should be a non-negative integer" } */
