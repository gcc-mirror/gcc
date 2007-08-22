/* { dg-do compile } */
/* This test checks for absolute memory operands.  */
/* { dg-require-effective-target nonpic } */
/* { dg-options "-O2 -march=k8" } */
/* { dg-final { scan-assembler "and\[^\\n\]*magic" } } */

/* Should be done as "andw $32767, magic".  */
static unsigned short magic;
void t(void)
{
	magic%=(unsigned short)0x8000U;
}
