/* { dg-do compile } */
/* { dg-options "-mabi=windowed" } */

#ifndef __XTENSA_WINDOWED_ABI__
#error
#endif

void foo(void)
{
}

/* { dg-final { scan-assembler "entry" } } */
/* { dg-final { scan-assembler "retw" } } */
