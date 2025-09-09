/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

void __attribute__((riscv_vector_cc))
f_try_sibcall_v2n_indirect (void (*func) (void))
{
  func ();
}

/* { dg-final { scan-assembler-times "\\.variant_cc\tf_try_sibcall_v2n_indirect\n" 1 } } */
/* { dg-final { scan-assembler-times "\tjalr\ta0\n" 1 } } */
/* { dg-final { scan-assembler-times "\tjr\tra\n" 1 } } */
