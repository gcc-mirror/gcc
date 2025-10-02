/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-march=rv64gc_xandesperf -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

char *ptr_ub;
char foo_ub (unsigned int a)
{
  return *(ptr_ub + a);
}

short *ptr_uh;
short foo_uh (unsigned int a)
{
  return *(ptr_uh + a);
}

int *ptr_ui;
int foo_ui (unsigned int a)
{
  return *(ptr_ui + a);
}

long long *ptr_ud;
long long foo_ud (unsigned int a)
{
  return *(ptr_ud + a);
}
/* { dg-final { scan-assembler-times {\mnds.lea.b.ze} 1 } } */
/* { dg-final { scan-assembler-times {\mnds.lea.h.ze} 1 } } */
/* { dg-final { scan-assembler-times {\mnds.lea.w.ze} 1 } } */
/* { dg-final { scan-assembler-times {\mnds.lea.d.ze} 1 } } */
