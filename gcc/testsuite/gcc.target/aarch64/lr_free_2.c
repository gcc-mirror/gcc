/* { dg-do run } */
/* { dg-options "-fno-inline -O2 -ffixed-x2 -ffixed-x3 -ffixed-x4 -ffixed-x5 -ffixed-x6 -ffixed-x7 -ffixed-x8 -ffixed-x9 -ffixed-x10 -ffixed-x11 -ffixed-x12 -ffixed-x13 -ffixed-x14 -ffixed-x15 -ffixed-x16 -ffixed-x17 -ffixed-x18 -ffixed-x19 -ffixed-x20 -ffixed-x21 -ffixed-x22 -ffixed-x23 -ffixed-x24 -ffixed-x25 -ffixed-x26 -ffixed-x27 -ffixed-x28 --save-temps -mgeneral-regs-only -fno-ipa-cp -fdump-rtl-ira" } */

extern void abort ();

int
cal (int a, int b)
{
  /* { dg-final { scan-assembler-times "stp\tx29, x30, \\\[sp, -\[0-9\]+\\\]!" 2 } } */
  int sum = a + b;
  int sum1 = a * b;
  /* { dg-final { scan-assembler-times "ldp\tx29, x30, \\\[sp\\\], \[0-9\]+" 2 } } */
  /* { dg-final { scan-rtl-dump "assign reg 30" "ira" } } */
  return (a + b + sum + sum1);
}

int
main (int argc, char **argv)
{
  int ret = cal (1, 2);

  if (ret != 8)
    abort ();

  return 0;
}

