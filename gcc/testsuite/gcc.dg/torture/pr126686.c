/* PR rtl-optimization/126686 */
/* { dg-do compile } */
/* { dg-additional-options "-favoid-store-forwarding" } */

int tmp;
short d_e;

int
foo ()
{
  long f = 0;
  __builtin_memset ((char *) &f + sizeof f - 2, d_e, 2);
  tmp = f;

  return f;
}
