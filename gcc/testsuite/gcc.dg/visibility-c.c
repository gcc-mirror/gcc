/* Test that visibility works on common symbols also. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-hidden "options" } } */

int options  __attribute__((__visibility__("hidden")));

void f(void)
{
  options = 0;
}
