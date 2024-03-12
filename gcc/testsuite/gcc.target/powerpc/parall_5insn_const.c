/* { dg-do run } */
/* { dg-options "-O2 -mno-prefixed -save-temps" } */
/* { dg-require-effective-target has_arch_ppc64 } */

/* { dg-final { scan-assembler-times {\mlis\M} 4 } } */
/* { dg-final { scan-assembler-times {\mori\M} 4 } } */
/* { dg-final { scan-assembler-times {\mrldimi\M} 2 } } */

void __attribute__ ((noinline)) foo (unsigned long long *a)
{
  /* 2 lis + 2 ori + 1 rldimi for each constant.  */
  *a++ = 0x800aabcdc167fa16ULL;
  *a++ = 0x7543a876867f616ULL;
}

long long A[] = {0x800aabcdc167fa16ULL, 0x7543a876867f616ULL};
int
main ()
{
  long long res[2];

  foo (res);
  if (__builtin_memcmp (res, A, sizeof (res)) != 0)
    __builtin_abort ();

  return 0;
}
