/* PR target/77587 */
/* { dg-do run } */
/* { dg-require-alias "" } */
/* { dg-require-weak-override "" } */
/* { dg-additional-sources "pr77587a.c" } */

void
bar (long x, long y, long z)
{
  struct __attribute__((aligned (16))) S { long a, b, c, d; } s;
  char *p = (char *) &s;
  __asm ("" : "+r" (p));
  if (((__UINTPTR_TYPE__) p) & 15)
    __builtin_abort ();
}
