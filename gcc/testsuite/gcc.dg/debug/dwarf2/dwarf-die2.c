/* Verify that inline function never actually emit has no DIE.  */
/* { dg-do compile */
/* { dg-options "-O0 -gdwarf-2 -dA" } */
/* { dg-final { scan-assembler-not "CIE Version" } } */
static inline int t()
{
}
