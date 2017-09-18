/* PR target/81300 */
/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O2" } */

int
__attribute__((noinline, noclone))
foo (void)
{
  unsigned long long _discard = 0, zero = 0, maxull = 0;
  unsigned char zero1 = __builtin_ia32_addcarryx_u64 (0, 0, 0, &_discard);
  unsigned char zero2 = __builtin_ia32_addcarryx_u64 (zero1, 0, 0, &zero);
  __builtin_ia32_sbb_u64 (0x0, 2, -1, &_discard);
  unsigned char one = __builtin_ia32_sbb_u64 (0, zero, 1, &maxull);
  unsigned long long x = __builtin_ia32_sbb_u64 (one, zero2, 0, &_discard);

  unsigned long long z1 = 0;
  __asm__ ("mov{q}\t{%1, %0|%0, %1}" : "+r" (z1) : "r" (x));
  unsigned long long z2 = 3;
  __asm__ ("mov{q}\t{%1, %0|%0, %1}" : "+r" (z2) : "r" (x));

  return 1 - (z1 | z2);
}

int main ()
{
  if (foo ())
    __builtin_abort ();

  return 0;
}
