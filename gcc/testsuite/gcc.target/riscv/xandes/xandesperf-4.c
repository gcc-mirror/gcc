/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xandesperf -mabi=lp64" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xandesperf -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

unsigned short foo (long a)
{
  return (unsigned short) a;
}

/* { dg-final { scan-assembler {\mnds.bfoz} } } */
