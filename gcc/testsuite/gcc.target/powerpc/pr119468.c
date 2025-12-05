/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-options "-O2 -mdejagnu-cpu=power7" } */

bool foo(unsigned long long x)
{
  return __builtin_parityll(x);
}

bool parity2(unsigned long long l)
{
  return __builtin_popcountll(l) & 1;
}

/* { dg-final { scan-assembler-times "popcntd" 2 } } */
/* { dg-final { scan-assembler-not "popcntb" } } */
/* { dg-final { scan-assembler-times "rldicl" 2 } } */
