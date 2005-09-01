/* Inlined non-inline function must have abstract DIE  */
/* { dg-do compile } */
/* { dg-options "-O2 -gdwarf-2 -dA -fpreprocessed" } */
/* { dg-final { scan-assembler "1.*DW_AT_inline" } } */
#1 "test.h"
void f(void);
static int t()
{
	f();
}
int q()
{
  t();
}
