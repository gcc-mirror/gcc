/* PR middle-end/98190 */
/* { dg-do run } */
/* { dg-options "-O2" } */

static int __attribute__((noipa))
foo (const char *p, const char *q, const int len)
{
  for (int i = 0; i < len; p++, q++, i++)
    {
      int equal;
      _Bool x, y;
      __builtin_memcpy ((char *) &x, p, sizeof x);
      __builtin_memcpy ((char *) &y, q, sizeof y);
      equal = (x == y);
      if (equal <= 0)
	return equal;
    }
  return 1;
}

int
main ()
{
  const _Bool buf[4] = { 1, 0, 0, 0 };
#ifdef __aarch64__
  register long x4 asm ("x4") = 0xdeadbeefULL;
  register long x5 asm ("x5") = 0xcafebabeULL;
  asm volatile (""::"r" (x4), "r" (x5));
#endif
  if (foo ((char *) &buf[0], (char *) &buf[0], 1) != 1)
    __builtin_abort ();
  return 0;
}
