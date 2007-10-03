/* { dg-mips-options "-mabi=32 -mfp64 -O2 -EB" } */

NOMIPS16 double
foo (double d)
{
  register double l1 asm ("$8") = d;
  register double l2 asm ("$f20") = 0.0;
  asm ("#foo" : "=d" (l1) : "d" (l1));
  asm volatile ("#foo" :: "f" (l2));
  return l1;
}

/* { dg-final { scan-assembler "\tmfc1\t\\\$9,\\\$f12\n" } } */
/* { dg-final { scan-assembler "\tmfhc1\t\\\$8,\\\$f12\n" } } */
/* { dg-final { scan-assembler "\tmtc1\t\\\$0,\\\$f20\n" } } */
/* { dg-final { scan-assembler "\tmthc1\t\\\$0,\\\$f20\n" } } */
/* { dg-final { scan-assembler "\tmtc1\t\\\$9,\\\$f0\n" } } */
/* { dg-final { scan-assembler "\tmthc1\t\\\$8,\\\$f0\n" } } */
