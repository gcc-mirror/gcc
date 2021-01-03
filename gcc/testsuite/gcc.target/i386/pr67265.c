/* PR target/67265 */
/* Reduced testcase by Johannes Dewender <gnu@JonnyJD.net> */

/* { dg-do compile } */
/* { dg-require-stack-check "" } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O -fstack-check -fPIC" } */

int a, b, c, d, e;

void foo (void)
{
  __asm__("" : "+r"(c), "+r"(e), "+r"(d), "+r"(a) : ""(b), "mg"(foo), "mm"(c));
}
