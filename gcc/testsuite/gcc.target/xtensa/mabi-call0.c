/* { dg-do compile } */
/* { dg-options "-mabi=call0" } */

#ifndef __XTENSA_CALL0_ABI__
#error
#endif

void foo(void)
{
}

/* { dg-final { scan-assembler-not "entry" } } */
/* { dg-final { scan-assembler-not "retw" } } */
