/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2" } */

typedef unsigned char vec_t __attribute__((vector_size(16)));

void
foo (__vector_pair *dst, vec_t *src)
{
  __vector_pair pair;
  __builtin_mma_assemble_pair (&pair, src[0], src[4]);
  *dst = pair;
}

void
foo2 (__vector_pair *dst, vec_t *src)
{
  __vector_pair pair;
  __builtin_vsx_assemble_pair (&pair, src[0], src[4]);
  *dst = pair;
}

void
bar (vec_t *dst, __vector_pair *src)
{
  vec_t res[2];
  __builtin_mma_disassemble_pair (res, src);
  dst[0] = res[0];
  dst[4] = res[1];
}

void
bar2 (vec_t *dst, __vector_pair *src)
{
  vec_t res[2];
  __builtin_vsx_disassemble_pair (res, src);
  dst[0] = res[0];
  dst[4] = res[1];
}

#if !__has_builtin (__builtin_vsx_assemble_pair)
#  error "__has_builtin (__builtin_vsx_assemble_pair) failed"
#endif

#if !__has_builtin (__builtin_vsx_disassemble_pair)
#  error "__has_builtin (__builtin_vsx_disassemble_pair) failed"
#endif

#if !__has_builtin (__builtin_mma_assemble_pair)
#  error "__has_builtin (__builtin_mma_assemble_pair) failed"
#endif

#if !__has_builtin (__builtin_mma_disassemble_pair)
#  error "__has_builtin (__builtin_mma_disassemble_pair) failed"
#endif

/* { dg-final { scan-assembler-times {\mlxv\M} 4 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstxv\M} 4 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 2 } } */

