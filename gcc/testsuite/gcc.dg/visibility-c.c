/* Test that visibility works on common symbols also. */
/* { dg-do assemble } */
/* { dg-require-visibility "" } */

int options  __attribute__((__visibility__("hidden")));

void f(void)
{
  options = 0;
}
