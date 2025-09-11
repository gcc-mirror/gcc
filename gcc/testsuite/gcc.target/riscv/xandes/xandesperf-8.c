/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xandesperf -mabi=lp64" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xandesperf -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

short *ptr_h;
short foo_h (long a)
{
  return *(ptr_h + a);
}

int *ptr_i;
int foo_i (long a)
{
  return *(ptr_i + a);
}

long long *ptr_d;
long long foo_d (long a)
{
  return *(ptr_d + a);
}

/* { dg-final { scan-assembler-times {\mnds.lea.h} 1 } } */
/* { dg-final { scan-assembler-times {\mnds.lea.w} 1 } } */
/* { dg-final { scan-assembler-times {\mnds.lea.d} 1 } } */
