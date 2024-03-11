/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-g" "-Oz" "-Os" } } */

int foo1(int rs1)
{
  return 100 & ~rs1;
}

int foo2(int rs1)
{
  return 100 | ~rs1;
}

/* { dg-final { scan-assembler-times "andn\t" 1 } } */
/* { dg-final { scan-assembler-times "orn\t" 1 } } */
/* { dg-final { scan-assembler-times "li\t" 2 } } */
