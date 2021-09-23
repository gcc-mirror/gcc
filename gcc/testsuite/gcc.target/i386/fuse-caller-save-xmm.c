/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-avx -fipa-ra -fomit-frame-pointer" } */

typedef double v2df __attribute__((vector_size (16)));

static v2df __attribute__((noinline))
bar (v2df a)
{
  return a + (v2df){ 3.0, 4.0 };
}

v2df __attribute__((noinline))
foo (v2df y)
{
  return y + bar (y);
}

/* Check presence of all insns on xmm registers.  These checks are expected to
   pass with both -fipa-ra and -fno-ipa-ra.  */

/* Darwin local constant symbol is "lC0", ELF targets ".LC0" */
/* { dg-final { scan-assembler-times {addpd\t\.?[Ll]C0.*, %xmm0} 1 { target { { ! ia32 } || nonpic } } } } */
/* { dg-final { scan-assembler-times {movapd\t\.?[Ll]C0.*, %xmm1} 1 { target { ia32 && { ! nonpic } } } } } */

/* We happen to get this for both cases, but in different positions.  */
/* { dg-final { scan-assembler-times "addpd\t%xmm1, %xmm0" 1 } } */

/* { dg-final { scan-assembler-times "movapd\t%xmm0, %xmm1" 1 { target { { ! ia32 } || nonpic } } } } */
/* { dg-final { scan-assembler-times "movapd\t%xmm0, %xmm2" 1 { target { ia32 && { ! nonpic } } } } } */
/* { dg-final { scan-assembler-times "addpd\t%xmm2, %xmm0" 1 { target { ia32 && { ! nonpic } } } } } */

/* Check absence of save/restore of xmm1 register.  */
/* { dg-final { scan-assembler-not "movaps\t%xmm1, \\(%\[re\]?sp\\)" } } */
/* { dg-final { scan-assembler-not "movapd\t\\(%\[re\]?sp\\), %xmm1" } } */
