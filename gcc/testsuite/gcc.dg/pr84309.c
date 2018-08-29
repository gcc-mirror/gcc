/* PR middle-end/84309 */
/* { dg-do run { target c99_runtime } } */
/* { dg-options "-O2 -ffast-math" } */

int
main ()
{
  unsigned long a = 1024;
  unsigned long b = 16 * 1024;
  unsigned long c = __builtin_pow (2, (__builtin_log2 (a) + __builtin_log2 (b)) / 2);
  if (c != 4096)
    __builtin_abort ();
  return 0;
}
