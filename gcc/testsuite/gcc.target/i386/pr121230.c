/* { dg-do compile { target ia32 } } */
/* { dg-options "-O3 -march=athlon-xp -mfpmath=387 -fexcess-precision=standard" } */

typedef struct {
    float a;
    float b;
} f32_2;

f32_2 add32_2(f32_2 x, f32_2 y) {
    return (f32_2){ x.a + y.a, x.b + y.b};
}

/* We do not want the vectorizer to vectorize the store and/or the
   conversion (with IA32 we do not support V2SF add) given that spills
   FP regs to reload them to XMM.  */
/* { dg-final { scan-assembler-not "movss\[ \\t\]+\[0-9\]*\\\(%esp\\\), %xmm" } } */
