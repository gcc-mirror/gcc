/* not inline inline function must not have abstract DIE  */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-inline -gdwarf-2 -dA -fpreprocessed" } */
/* { dg-final { scan-assembler-not "DW_AT_inline" } } */
#1 "test.h"
inline int t()
{
}
int q()
{
  t();
}
