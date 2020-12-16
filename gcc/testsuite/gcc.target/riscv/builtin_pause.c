/* { dg-do compile } */
/* { dg-options "-O2" }  */

void test_pause()
{
  __builtin_riscv_pause ();
}

/* { dg-final { scan-assembler "pause" } } */

