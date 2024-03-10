/* { dg-do compile } */
/* { dg-options "-march=rv32i_zihintpause -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

void
test ()
{
  __builtin_riscv_pause ();
}

/* { dg-final { scan-assembler-times "\tpause" 1 } } */
