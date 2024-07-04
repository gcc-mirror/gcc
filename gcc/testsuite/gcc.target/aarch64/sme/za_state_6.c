// { dg-options "-O -fno-optimize-sibling-calls -fomit-frame-pointer" }

void private_za();
void out_za() __arm_out("za");
void in_za() __arm_in("za");

__arm_new("za") void test20(volatile int *ptr)
{
  if (*ptr)
    out_za();
  else
    *ptr += 1;
  *ptr += 1;
  if (*ptr)
    in_za();
  else
    *ptr += 1;
}

// { dg-final { scan-assembler {\tbl\t__arm_tpidr2_save\n} } }
// { dg-final { scan-assembler {\tsmstart\tza\n} } }
// { dg-final { scan-assembler {\tsmstop\tza\n} } }
// { dg-final { scan-assembler-not {\tsub\tsp, sp, x[0-9]+\n} } }
