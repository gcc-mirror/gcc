/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -mx32" } */

void
test1 (void)
{
  int* apic_tpr_addr = (int *) 0xfee00080;
  *apic_tpr_addr += 4;
}

void
test2 (void)
{
  int* apic_tpr_addr = (int *) 0xfee00080;
  *apic_tpr_addr = 0;
}

/* { dg-final { scan-assembler-not "\[,\\t \]+-18874240\n" } } */
