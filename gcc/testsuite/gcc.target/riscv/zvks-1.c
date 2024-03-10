/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvksed_zvksh_zvbb_zvkt" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zvksed_zvksh_zvbb_zvkt" { target { rv32 } } } */

#ifndef __riscv_zvks
#error Feature macro not defined
#endif

#ifndef __riscv_zvksed
#error Feature macro not defined
#endif

#ifndef __riscv_zvksh
#error Feature macro not defined
#endif

#ifndef __riscv_zvbb
#error Feature macro not defined
#endif

#ifndef __riscv_zvkt
#error Feature macro not defined
#endif

int
foo (int a)
{
  return a;
}
