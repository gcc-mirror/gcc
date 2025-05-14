/* PR target/119450 */
/* { dg-do compile } */
/* { dg-options "-O3" } */

long *a;
int b;

void
foo (void)
{
  unsigned d = b >> 30;
  a = (long *) (__UINTPTR_TYPE__) d;
  if (*a & 1 << 30)
    *a = 0;
}
