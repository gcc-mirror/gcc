/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbkb -mabi=lp64" { target rv64 } } */
/* { dg-options "-O2 -march=rv32gc_zbkb -mabi=ilp32" { target rv32 } } */

unsigned long
foo (unsigned long x)
{
  return (x << 1) | ((long) x >= 0);
}

/* { dg-final { scan-assembler {\mrori\M} } } */
/* { dg-final { scan-assembler {\mxori\M} } } */
