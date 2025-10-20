/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xandesperf -mabi=lp64" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xandesperf -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

long foo (long cond)
{
  if (cond != 63)
    cond += 10;
  return cond;
}

/* { dg-final { scan-assembler {\mnds.b..c} } } */
