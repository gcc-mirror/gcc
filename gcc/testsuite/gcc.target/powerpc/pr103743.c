/* { dg-options "-O2" } */
/* { dg-do compile { target has_arch_ppc64 } } */

/* { dg-final { scan-assembler-times {\mcmpldi\M} 10  } } */
/* { dg-final { scan-assembler-times {\mcmpdi\M} 4  } } */
/* { dg-final { scan-assembler-times {\mrotldi\M} 14  } } */

int foo (int a);

int __attribute__ ((noinline)) udi_fun (unsigned long long in)
{
  if (in == (0x8642000000000000ULL))
    return foo (1);
  if (in == (0x7642000000000000ULL))
    return foo (12);
  if (in == (0x8000000000000000ULL))
    return foo (32);
  if (in == (0x8700000000000091ULL))
    return foo (33);
  if (in == (0x8642FFFFFFFFFFFFULL))
    return foo (46);
  if (in == (0x7642FFFFFFFFFFFFULL))
    return foo (51);
  if (in == (0x7567000000ULL))
    return foo (9);
  if (in == (0xFFF8567FFFFFFFFFULL))
    return foo (19);

  return 0;
}

int __attribute__ ((noinline)) di_fun (long long in)
{
  if (in == (0x8642000000000000LL))
    return foo (1);
  if (in == (0x7642000000000000LL))
    return foo (12);
  if (in == (0x8000000000000000LL))
    return foo (32);
  if (in == (0x8700000000000091LL))
    return foo (33);
  if (in == (0x8642FFFFFFFFFFFFLL))
    return foo (46);
  if (in == (0x7642FFFFFFFFFFFFLL))
    return foo (51);
  if (in == (0x7567000000LL))
    return foo (9);
  if (in == (0xFFF8567FFFFFFFFFLL))
    return foo (19);

  return 0;
}
