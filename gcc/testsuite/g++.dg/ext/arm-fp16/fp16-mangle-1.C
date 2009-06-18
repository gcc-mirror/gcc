/* { dg-do compile { target arm*-*-* } } */
/* { dg-options "-mfp16-format=ieee" } */

/* Test mangling */

/* { dg-final { scan-assembler "\t.global\t_Z1fPDh" } } */
void f (__fp16 *x) { }

/* { dg-final { scan-assembler "\t.global\t_Z1gPDhS_" } } */
void g (__fp16 *x, __fp16 *y) { }

/* { dg-final { scan-assembler "\t.global\t_ZN1SIDhDhE1iE" } } */
template <typename T, typename U> struct S { static int i; }; 
template <> int S<__fp16, __fp16>::i = 3;
