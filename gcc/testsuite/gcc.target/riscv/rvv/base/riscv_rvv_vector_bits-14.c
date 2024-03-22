/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

void test () {

#if __riscv_v_fixed_vlen != 128
#error "__riscv_v_fixed_vlen should be 128"
#endif

}
