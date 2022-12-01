/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* bclri + bclri */
long long f5 (long long a)
{
  return a & ~0x11000;
}

/* { dg-final { scan-assembler-times "bclri\t" 2 } } */

