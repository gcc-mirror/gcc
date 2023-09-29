/* { dg-do compile } */
/* { dg-options "-march=rv64i_zihintpause -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

void
test ()
{
  __builtin_riscv_pause ();
}

/* { dg-final { scan-assembler-times "\tpause" 1 } } */
