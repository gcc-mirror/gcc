/* Test to make sure the macro info includes the predefined macros with line number 0.  */
/* { dg-do compile } */
/* { dg-options "-g3 -gdwarf-2 -dA -fverbose-asm" } */
/* { dg-final { scan-assembler "At line number 0" } } */

#define FOO 1
int i;
