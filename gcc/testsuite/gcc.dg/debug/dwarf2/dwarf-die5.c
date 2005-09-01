/* Inlined inline function must have abstract DIE  */
/* { dg-do compile } */
/* { dg-options "-O2 -gdwarf-2 -dA -fpreprocessed" } */
/* { dg-final { scan-assembler "3.*DW_AT_inline" } } */
#1 "test.h"
inline int t()
{
}
int q()
{
  t();
}
