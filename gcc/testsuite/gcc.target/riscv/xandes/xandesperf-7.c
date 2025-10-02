/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xandesperf -mabi=lp64" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xandesperf -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

long foo (long a)
{
  return (a & 0xffe0) >> 5;
}

unsigned long foo1 (unsigned long a)
{
  return (a & 0xffe0) >> 5;
}

signed char foo2 (long a)
{
  return (signed char) ((a & 0xff00) >> 8);
}

/* { dg-final { scan-assembler-times {\mnds.bfoz} 2 } } */
/* { dg-final { scan-assembler-times {\mnds.bfos} 1 } } */
