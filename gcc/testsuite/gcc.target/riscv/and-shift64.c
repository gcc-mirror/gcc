/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-g" } } */

long long foo(long long a)
{
  return (a << 8) & 24320;
}

/* { dg-final { scan-assembler-times "\\sandi\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sslli\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\sli\\s" } } */
/* { dg-final { scan-assembler-not "\\saddi\\s" } } */