/* { dg-do compile } */
/* { dg-options "isa>=2 -mabi=32" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "addiu" } } */
/* { dg-final { scan-assembler-not "subu" } } */

NOMIPS16 unsigned long
f(unsigned long *p)
{
  return __sync_fetch_and_sub (p, 5);
}
