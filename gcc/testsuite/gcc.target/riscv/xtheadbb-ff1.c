/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv32gc_xtheadbb" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadbb" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" } } */

int
foo (unsigned long a)
{
  return __builtin_clzl (a);
}

int
bar (unsigned long a)
{
  return __builtin_ctzl (a);
}

/* { dg-final { scan-assembler-times "th.ff1\t" 2 } } */
