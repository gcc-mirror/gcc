/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -mhotpatch=1000001,1000000" } */

viod main(void)
{
  return 0;
}

/* { dg-error "argument to .-mhotpatch=n,m. is too large" "" { target *-*-* } 0 } */
