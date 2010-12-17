/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

#define vector __attribute__((vector_size(16)))

char a;
vector char f(void) { return (vector char){ a, a, a, a, a, a, a, a,
					    a, a, a, a, a, a, a, a }; }
/* { dg-final { scan-assembler-not "sall" } } */
/* { dg-final { scan-assembler-not "%mm" } } */
