/* PR target/102639 */
/* { dg-do compile } */
/* { dg-options "-O2 -std=c++14 -mavx512fp16" } */
/* { dg-final { scan-assembler-times "vminsh" 1 } } */

typedef _Float16 v16hf __attribute__((vector_size(2)));
v16hf vcond_v16hfv16hfge_b, vcond_v16hfv16hfge_c, vcond_v16hfv16hfge_d,
    __attribute__vcond_v16hfv16hfge_a;
v16hf __attribute__vcond_v16hfv16hfge() {
  return __attribute__vcond_v16hfv16hfge_a >= vcond_v16hfv16hfge_b
             ? vcond_v16hfv16hfge_c
             : vcond_v16hfv16hfge_d;
}

v16hf __attribute__vcond_v16hfv16hfmax() {
  return __attribute__vcond_v16hfv16hfge_a < vcond_v16hfv16hfge_b
             ? __attribute__vcond_v16hfv16hfge_a
             : vcond_v16hfv16hfge_b;
}
