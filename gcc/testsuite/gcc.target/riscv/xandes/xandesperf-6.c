/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xandesperf -mabi=lp64" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xandesperf -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

short foo (long a)
{
  short lala = a;
  return lala;
}

signed char foo1 (long a)
{
  signed char lala = a;
  return lala;
}

/* { dg-final { scan-assembler-times {\mnds.bfos} 2 } } */
