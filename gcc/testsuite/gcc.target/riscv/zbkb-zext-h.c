/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc_zbkb -mabi=ilp32d" { target rv32 } } */
/* { dg-options "-O2 -march=rv64gc_zbkb -mabi=lp64d" { target rv64 } } */

unsigned long
zext_h (unsigned long x)
{
  return (unsigned short) x;
}

/* { dg-final { scan-assembler {\mzext\.h\M} } } */
