/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch" } */

__attribute__((hotpatch(0)))
int main (void)
{/* { dg-error "wrong number of arguments specified" } */
  return 0;
}
