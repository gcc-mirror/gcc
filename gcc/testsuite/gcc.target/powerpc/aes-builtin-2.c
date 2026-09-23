/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

typedef unsigned char vec_t __attribute__((vector_size(16)));

void
gfmul (vec_t *a, vec_t *b, vec_t *res)
{
  vec_t A = *a;
  vec_t B = *b;
  vec_t R = __builtin_galois_field_mult (A, B, 0);
  R = __builtin_galois_field_mult (R, B, 1);
  *res = R;
}
void
gfmul_gcm (vec_t *a, vec_t *b, vec_t *res)
{
  vec_t A = *a;
  vec_t B = *b;
  vec_t R = __builtin_galois_field_mult_gcm (A, B);
  *res = R;
}
void
gfmul_xts (vec_t *a, vec_t *b, vec_t *res)
{
  vec_t A = *a;
  vec_t B = *b;
  vec_t R = __builtin_galois_field_mult_xts (A, B);
  *res = R;
}

/* { dg-final { scan-assembler-times {\mxxgfmul128\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxgfmul128gcm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxgfmul128xts\M} 1 } } */
