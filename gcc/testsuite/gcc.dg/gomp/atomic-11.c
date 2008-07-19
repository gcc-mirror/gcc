/* PR middle-end/36877 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */
/* { dg-options "-fopenmp -march=i386" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

int i;
float f;

void foo (void)
{
#pragma omp atomic
  i++;
#pragma omp atomic
  f += 1.0;
}

/* { dg-final { scan-assembler-not "__sync_(fetch|add|bool|val)" { target i?86-*-* x86_64-*-* powerpc*-*-* ia64-*-* s390*-*-* sparc*-*-* } } } */
