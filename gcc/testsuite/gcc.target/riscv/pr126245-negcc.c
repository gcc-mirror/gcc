/* PR target/126245.  Test the RISC-V negcc expander through if-conversion.  */
/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-O2 -march=rv64g -mabi=lp64d -fdump-rtl-ce1" { target { rv64 } } } */
/* { dg-options "-O2 -march=rv32g -mabi=ilp32d -fdump-rtl-ce1" { target { rv32 } } } */

long
f_negcc (long x, long y)
{
  return x < y ? 42 : -42;
}

/* { dg-final { scan-assembler-times {\mslt\M} 1 } } */
/* { dg-final { scan-assembler-times {\m(?:xor|xori)\M} 1 } } */
/* { dg-final { scan-assembler-times {\madd\M} 1 } } */
/* { dg-final { scan-assembler-not {\m(?:beq|bne|blt|bge|ble|bgt)(?:u|z)?\M} } } */
/* { dg-final { scan-rtl-dump-times {if-conversion succeeded through noce_try_inverse_constants} 1 "ce1" } } */
