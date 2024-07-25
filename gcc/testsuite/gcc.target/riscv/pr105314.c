/* PR rtl-optimization/105314 */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-march=*zicond*" "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-fdump-rtl-ce1" } */

long
foo (long a, long b, long c)
{
  if (c)
    a = 0;
  return a;
}

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_store_flag_mask" 1 "ce1" } } */
/* { dg-final { scan-assembler-not "\\s(?:beq|bne)\\s" } } */
