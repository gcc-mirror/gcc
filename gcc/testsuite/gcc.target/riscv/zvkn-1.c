/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvkned_zvknhb_zvbb_zvkt" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zvkned_zvknhb_zvbb_zvkt" { target { rv32 } } } */

#ifndef __riscv_zvkn
#error Feature macro not defined
#endif

#ifndef __riscv_zvkned
#error Feature macro not defined
#endif

#ifndef __riscv_zvknhb
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
