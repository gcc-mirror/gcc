/* Verify that inline function never actually inlined has no abstract DIE.  */
/* { dg-do compile */
/* { dg-options "-O2 -gdwarf-2 -dA" } */
/* { dg-final { scan-assembler-not "DW_AT_inline" } } */
inline int t()
{
}
int (*q)()=t;
