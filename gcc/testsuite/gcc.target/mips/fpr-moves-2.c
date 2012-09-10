/* { dg-options "-mabi=32 -mhard-float -mips1 -EB" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

NOMIPS16 void
foo (double d, double *x)
{
  *x = d;
}

NOMIPS16 double
bar (double d)
{
  register double l1 asm ("$8") = d;
  register double l2 asm ("$f20") = 0.0;
  asm ("#foo" : "=d" (l1) : "d" (l1));
  asm volatile ("#foo" :: "f" (l2));
  return l1;
}

/* { dg-final { scan-assembler "\tswc1\t\\\$f12,4\\\(\\\$6\\\)\n" } } */
/* { dg-final { scan-assembler "\tswc1\t\\\$f13,0\\\(\\\$6\\\)\n" } } */
/* { dg-final { scan-assembler "\tmfc1\t\\\$9,\\\$f12\n" } } */
/* { dg-final { scan-assembler "\tmfc1\t\\\$8,\\\$f13\n" } } */
/* { dg-final { scan-assembler "\tmtc1\t\\\$0,\\\$f20\n" } } */
/* { dg-final { scan-assembler "\tmtc1\t\\\$0,\\\$f21\n" } } */
/* { dg-final { scan-assembler "\tmtc1\t\\\$9,\\\$f0\n" } } */
/* { dg-final { scan-assembler "\tmtc1\t\\\$8,\\\$f1\n" } } */
