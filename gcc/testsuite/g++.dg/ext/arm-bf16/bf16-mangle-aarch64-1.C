/* { dg-do compile { target aarch64*-*-* } } */

/* Test mangling */

/* { dg-final { scan-assembler "\t.global\t_Z1fPu6__bf16" } } */
void f (__bf16 *x) { }

/* { dg-final { scan-assembler "\t.global\t_Z1gPu6__bf16S_" } } */
void g (__bf16 *x, __bf16 *y) { }

/* { dg-final { scan-assembler "\t.global\t_ZN1SIu6__bf16u6__bf16E1iE" } } */
template <typename T, typename U> struct S { static int i; };
template <> int S<__bf16, __bf16>::i = 3;
