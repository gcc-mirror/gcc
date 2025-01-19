/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv64gc_zvkn_zvbc" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zvkn_zvbc" { target { rv32 } } } */

#ifndef __riscv_zvknc
#error "Feature macro for `Zvknc' not defined"
#endif

#ifndef __riscv_zvkn
#error "Feature macro for `Zvkn' not defined"
#endif

#ifndef __riscv_zvkned
#error "Feature macro for `Zvkned' not defined"
#endif

#ifndef __riscv_zvknhb
#error "Feature macro for `Zvknhb' not defined"
#endif

#ifndef __riscv_zvkb
#error "Feature macro for `Zvkb' not defined"
#endif

#ifndef __riscv_zvkt
#error "Feature macro for `Zvkt' not defined"
#endif

#ifndef __riscv_zvbc
#error "Feature macro for `Zvbc' not defined"
#endif

int
foo (int a)
{
  return a;
}
