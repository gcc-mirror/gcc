/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch" } */

__attribute__((hotpatch(-1,0)))
int main (void)
{/* { dg-error "attribute is not a comma separated pair of non-negative integer constants or too large" } */
  return 0;
}
