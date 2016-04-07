/* { dg-do compile } */
/* { dg-options "-fdump-tree-optimized" } */

extern
#ifdef __cplusplus
"C"
#endif
__attribute__((__simd__))
int simd_attr (void)
{
  return 0;
}

/* { dg-final { scan-tree-dump "simd_attr\[ \\t\]simdclone|vector" "optimized" { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN4_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbM4_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN4_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM4_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN8_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM8_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN16_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM16_simd_attr:" 1 { target { i?86-*-* x86_64-*-* } } } } */

extern
#ifdef __cplusplus
"C"
#endif
__attribute__((simd))
int simd_attr2 (void)
{
  return 0;
}

/* { dg-final { scan-tree-dump "simd_attr2\[ \\t\]simdclone|vector" "optimized" { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN4_simd_attr2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbM4_simd_attr2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN4_simd_attr2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM4_simd_attr2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN8_simd_attr2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM8_simd_attr2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN16_simd_attr2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM16_simd_attr2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
