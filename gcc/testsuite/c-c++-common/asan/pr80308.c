/* PR sanitizer/80308 */
/* { dg-do run } */

__attribute__((noinline, noclone)) int
foo (char *a)
{
  int i, j = 0;
  asm volatile ("" : "+r" (a) : : "memory");
  for (i = 0; i < 12; i++)
    j += a[i];
  return j;
}

int
main ()
{
  int i, j = 0;
  for (i = 0; i < 4; i++)
    {
      char a[12];
      __builtin_memset (a, 0, sizeof (a));
      j += foo (a);
    }
  return j;
}
