/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

void f_ext_n2n (void);
void f_ext_v2n (void);
void __attribute__((riscv_vector_cc)) f_ext_n2v (void);
void __attribute__((riscv_vector_cc)) f_ext_v2v (void);

/* { dg-final { scan-assembler-times "\\.variant_cc\tf_ext_n2n\n" 0 } } */
/* { dg-final { scan-assembler-times "\\.variant_cc\tf_ext_v2n\n" 0 } } */
/* { dg-final { scan-assembler-times "\\.variant_cc\tf_ext_n2v\n" 1 } } */
/* { dg-final { scan-assembler-times "\\.variant_cc\tf_ext_v2v\n" 1 } } */

void
f_try_sibcall_n2n (void)
{
  f_ext_n2n ();
}

/* { dg-final { scan-assembler-times "\\.variant_cc\tf_try_sibcall_n2n\n" 0 } } */
/* { dg-final { scan-assembler-times "\ttail\tf_ext_n2n\n" 1 } } */
/* { dg-final { scan-assembler-times "\tcall\tf_ext_n2n\n" 0 } } */

void
f_try_sibcall_n2v (void)
{
  f_ext_n2v ();
}

/* { dg-final { scan-assembler-times "\\.variant_cc\tf_try_sibcall_n2v\n" 0 } } */
/* { dg-final { scan-assembler-times "\ttail\tf_ext_n2v\n" 1 } } */
/* { dg-final { scan-assembler-times "\tcall\tf_ext_n2v\n" 0 } } */

void __attribute__((riscv_vector_cc))
f_try_sibcall_v2n (void)
{
  /* Vector to normal: sibling call optimization shall be
     suppressed to preserve caller's registers: v1-v7 and v24-v31.  */
  f_ext_v2n ();
}

/* { dg-final { scan-assembler-times "\\.variant_cc\tf_try_sibcall_v2n\n" 1 } } */
/* { dg-final { scan-assembler-times "\ttail\tf_ext_v2n\n" 0 } } */
/* { dg-final { scan-assembler-times "\tcall\tf_ext_v2n\n" 1 } } */

void __attribute__((riscv_vector_cc))
f_try_sibcall_v2v (void)
{
  f_ext_v2v ();
}

/* { dg-final { scan-assembler-times "\\.variant_cc\tf_try_sibcall_v2v\n" 1 } } */
/* { dg-final { scan-assembler-times "\ttail\tf_ext_v2v\n" 1 } } */
/* { dg-final { scan-assembler-times "\tcall\tf_ext_v2v\n" 0 } } */
