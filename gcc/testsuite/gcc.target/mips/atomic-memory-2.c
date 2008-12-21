/* { dg-do compile } */
/* { dg-options "-O2 isa>=2 -mabi=32" } */
/* { dg-final { scan-assembler "addiu" } } */
/* { dg-final { scan-assembler-not "subu" } } */

NOMIPS16 unsigned long
f(unsigned long *p)
{
    return __sync_fetch_and_sub (p, 5);
}
