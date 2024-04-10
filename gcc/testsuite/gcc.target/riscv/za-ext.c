/* { dg-do compile } */
/* { dg-options "-march=rv64gc_za64rs_za128rs" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_za64rs_za128rs" { target { rv32 } } } */

#ifndef __riscv_za64rs
#error "Feature macro for 'za64rs' not defined"
#endif

#ifndef __riscv_za128rs
#error "Feature macro for 'za128rs' not defined"
#endif

int
foo (int a)
{
  return a;
}
