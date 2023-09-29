/* { dg-do compile } */
/* { dg-options "-march=rv64i -mabi=lp64"  { target { rv64 } } } */
/* { dg-options "-march=rv32i -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

void
test ()
{
  __builtin_riscv_pause ();
}

/* { dg-final { scan-assembler-times "0x0100000f" 1 } } */
