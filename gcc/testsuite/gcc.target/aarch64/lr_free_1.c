/* { dg-do run } */
/* { dg-options "-fno-inline -O2 -fomit-frame-pointer -ffixed-x2 -ffixed-x3 -ffixed-x4 -ffixed-x5 -ffixed-x6 -ffixed-x7 -ffixed-x8 -ffixed-x9 -ffixed-x10 -ffixed-x11 -ffixed-x12 -ffixed-x13 -ffixed-x14 -ffixed-x15 -ffixed-x16 -ffixed-x17 -ffixed-x18 -ffixed-x19 -ffixed-x20 -ffixed-x21 -ffixed-x22 -ffixed-x23 -ffixed-x24 -ffixed-x25 -ffixed-x26 -ffixed-x27 -ffixed-28 -ffixed-29 --save-temps -mgeneral-regs-only -fno-ipa-cp -fno-schedule-fusion -fno-peephole2 --param=aarch64-stp-policy=never" } */

extern void abort ();

int
dec (int a, int b)
{
  return a + b;
}

int
cal (int a, int b)
{
  int sum1 = a * b;
  int sum2 = a / b;
  int sum = dec (sum1, sum2);
  return a + b + sum + sum1 + sum2;
}

int
main (int argc, char **argv)
{
  int ret = cal (2, 1);

  if (ret != 11)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "str\tx30, \\\[sp, -\[0-9\]+\\\]!" 2 } } */
/* { dg-final { scan-assembler "str\tw30, \\\[sp, \[0-9\]+\\\]" } } */

/* { dg-final { scan-assembler "ldr\tw30, \\\[sp, \[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler-times "ldr\tx30, \\\[sp\\\], \[0-9\]+" 2 } } */

