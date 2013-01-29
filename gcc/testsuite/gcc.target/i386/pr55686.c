/* PR target/55686 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (long x, long *y)
{
  long *a = y - 64, i;
  for (i = 0; i < x; i++)
    {
      long v = y[i];
      *a++ = v;
    }
  register void **c __asm__ ("di");
  goto **c;
}
