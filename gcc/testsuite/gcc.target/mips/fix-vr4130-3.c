/* { dg-do compile { target mips*-*-* } } */
/* { dg-skip-if "" { mips-sgi-irix* } { "-mabi=32" } { "" } } */
/* { dg-options "-mfix-vr4130 -march=vr4130" } */
#if _MIPS_ARCH_VR4130 && !__mips16 && __mips64
long long foo (void) { long long r; asm ("# foo" : "=h" (r)); return r; }
#else
asm ("#\tdmacchi\t");
#endif
/* { dg-final { scan-assembler "\tdmacchi\t" } } */
