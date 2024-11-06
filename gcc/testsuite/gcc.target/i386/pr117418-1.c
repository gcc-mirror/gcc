/* PR target/117418 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-maddress-mode=long -mwidekl -mx32" } */
/* { dg-require-effective-target maybe_x32  } */
/* { dg-final { scan-assembler-times "aesdec128kl" 1 } } */
/* { dg-final { scan-assembler-times "aesdec256kl" 1 } } */
/* { dg-final { scan-assembler-times "aesenc128kl" 1 } } */
/* { dg-final { scan-assembler-times "aesenc256kl" 1 } } */
/* { dg-final { scan-assembler-times "encodekey128" 1 } } */
/* { dg-final { scan-assembler-times "encodekey256" 1 } } */

typedef __attribute__((__vector_size__(16))) long long V;
V a;

void
foo()
{
    __builtin_ia32_aesdec128kl_u8 (&a, a, &a);
    __builtin_ia32_aesdec256kl_u8 (&a, a, &a);
    __builtin_ia32_aesenc128kl_u8 (&a, a, &a);
    __builtin_ia32_aesenc256kl_u8 (&a, a, &a);
    __builtin_ia32_encodekey128_u32 (0, a, &a); 
    __builtin_ia32_encodekey256_u32 (0, a, a, &a); 
}
