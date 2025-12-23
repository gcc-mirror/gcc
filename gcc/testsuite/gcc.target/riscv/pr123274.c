/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64id" { target rv64 } } */
/* { dg-options "-O2 -march=rv32id" { target rv32 } } */
unsigned a, b;

void
foo()
{
  if (__builtin_mul_overflow(a, b, &b))
    b = 0xffffffff;
}
