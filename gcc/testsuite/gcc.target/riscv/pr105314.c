/* PR rtl-optimization/105314 */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */

long
foo (long a, long b, long c)
{
  if (c)
    a = 0;
  return a;
}

/* { dg-final { scan-assembler-not "\\s(?:beq|bne)\\s" } } */
