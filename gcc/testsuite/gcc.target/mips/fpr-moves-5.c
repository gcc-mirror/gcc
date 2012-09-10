/* { dg-options "-mabi=64 -mhard-float -EL" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-require-effective-target mips_newabi_large_long_double } */

NOMIPS16 void
foo (long double d, long double *x)
{
  *x = d;
}

NOMIPS16 long double
bar (long double d, long double *x)
{
  register long double l1 asm ("$8") = d;
  register long double l2 asm ("$10") = x[1];
  register long double l3 asm ("$f20") = 0.0;
  asm ("#foo" : "=d" (l1) : "d" (l1));
  asm ("#foo" : "=d" (l2) : "d" (l2));
  asm volatile ("#foo" :: "f" (l3));
  x[1] = l1;
  return l2;
}

/* { dg-final { scan-assembler "\tsdc1\t\\\$f12,0\\\(\\\$6\\\)\n" } } */
/* { dg-final { scan-assembler "\tsdc1\t\\\$f13,8\\\(\\\$6\\\)\n" } } */
/* { dg-final { scan-assembler "\tdmfc1\t\\\$8,\\\$f12\n" } } */
/* { dg-final { scan-assembler "\tdmfc1\t\\\$9,\\\$f13\n" } } */
/* { dg-final { scan-assembler "\tld\t\\\$10,16\\\(\\\$6\\\)\n" } } */
/* { dg-final { scan-assembler "\tld\t\\\$11,24\\\(\\\$6\\\)\n" } } */
/* { dg-final { scan-assembler "\tdmtc1\t\\\$0,\\\$f20\n" } } */
/* { dg-final { scan-assembler "\tdmtc1\t\\\$0,\\\$f21\n" } } */
/* { dg-final { scan-assembler "\tsd\t\\\$8,16\\\(\\\$6\\\)\n" } } */
/* { dg-final { scan-assembler "\tsd\t\\\$9,24\\\(\\\$6\\\)\n" } } */
/* { dg-final { scan-assembler "\tdmtc1\t\\\$10,\\\$f0\n" } } */
/* { dg-final { scan-assembler "\tdmtc1\t\\\$11,\\\$f2\n" } } */
