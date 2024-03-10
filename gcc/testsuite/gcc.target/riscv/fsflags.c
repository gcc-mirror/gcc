/* Verify that fsflags is using the correct register or immediate.  */
/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-O" } */

void foo1 (int a)
{
   __builtin_riscv_fsflags(a);
}
void foo2 ()
{
   __builtin_riscv_fsflags(4);
}

/* { dg-final { scan-assembler-times "fsflags\t" 1 } } */
/* { dg-final { scan-assembler-times "fsflagsi\t" 1 } } */
