
/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -mrvv-vector-bits=scalable -O3" } */

void test () {

#if defined __riscv_v_fixed_vlen
#error "__riscv_v_fixed_vlen should not be defined"
#endif

}
