/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvksg" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zvksg" { target { rv32 } } } */

#ifndef __riscv_zvksg
#error Feature macro not defined
#endif

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

#ifndef __riscv_zvkg
#error Feature macro not defined
#endif

int
foo (int a)
{
  return a;
}
