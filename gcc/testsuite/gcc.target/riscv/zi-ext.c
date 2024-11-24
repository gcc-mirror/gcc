/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv64gc_zic64b_ziccamoa_ziccif_zicclsm_ziccrse" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zic64b_ziccamoa_ziccif_zicclsm_ziccrse" { target { rv32 } } } */

#ifndef __riscv_zic64b
#error "Feature macro for 'zic64b' not defined"
#endif

#ifndef __riscv_ziccamoa
#error "Feature macro for 'ziccamoa' not defined"
#endif

#ifndef __riscv_ziccif
#error "Feature macro for 'ziccif' not defined"
#endif

#ifndef __riscv_zicclsm
#error "Feature macro for 'zicclsm' not defined"
#endif

#ifndef __riscv_ziccrse
#error "Feature macro for 'ziccrse' not defined"
#endif

int
foo (int a)
{
  return a;
}
