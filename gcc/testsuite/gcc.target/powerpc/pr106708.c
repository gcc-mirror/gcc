/* PR target/106708 */
/* { dg-do run } */
/* { dg-options "-O2 -mno-prefixed -save-temps" } */
/* { dg-require-effective-target has_arch_ppc64 } */

long long arr[]
  = {0xffffffff7cdeab55LL, 0x98765432LL, 0xabcd0000LL};

void __attribute__ ((__noipa__)) test_li_xoris (long long *arg)
{
  *arg = 0xffffffff7cdeab55LL;
}
/* { dg-final { scan-assembler-times {\mli .*,-21675\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxoris .*0x8321\M} 1 } } */

void __attribute__ ((__noipa__)) test_li_oris (long long *arg)
{
  *arg = 0x98765432LL;
}
/* { dg-final { scan-assembler-times {\mli .*,21554\M} 1 } } */
/* { dg-final { scan-assembler-times {\moris .*0x9876\M} 1 } } */

void __attribute__ ((__noipa__)) test_lis_rldicl (long long *arg)
{
  *arg = 0xabcd0000LL;
}
/* { dg-final { scan-assembler-times {\mlis .*,0xabcd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mrldicl .*,0,32\M} 1 } } */

int
main ()
{
  long long a[sizeof (arr) / sizeof (arr[0])];

  test_li_xoris (a);
  test_li_oris (a + 1);
  test_lis_rldicl (a + 2);
  if (__builtin_memcmp (a, arr, sizeof (arr)) != 0)
    __builtin_abort ();
  return 0;
}
