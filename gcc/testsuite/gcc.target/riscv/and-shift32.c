/* { dg-do compile } */
/* { dg-options "-march=rv32gc -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-g" } } */

int foo(int a)
{
  return (a << 8) & 24320;
}

/* { dg-final { scan-assembler-times "\\sandi\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sslli\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\sli\\s" } } */
/* { dg-final { scan-assembler-not "\\saddi\\s" } } */
