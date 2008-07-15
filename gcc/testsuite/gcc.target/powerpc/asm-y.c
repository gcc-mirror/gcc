/* { dg-do compile } */
/* { dg-options "-O1" } */

/* Test that %yN does not cause an internal error if used incorrectly.  */

int f(int *a)
{
  asm ("#%y0" : "=m"(a[2])); /* { dg-error "try using the 'Z' constraint" } */
  asm ("#%y0" : "=m"(a[1])); /* { dg-error "try using the 'Z' constraint" } */
  asm ("#%y0" : "=m"(a[0]));
}
