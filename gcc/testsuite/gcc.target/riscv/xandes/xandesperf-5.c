/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-march=rv64gc_xandesperf -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

long foo (long a)
{
  int lala = a;
  return lala;
}

/* { dg-final { scan-assembler {\mnds.bfos} } } */
