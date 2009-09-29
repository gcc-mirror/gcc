/* { dg-do compile } */
/* { dg-options "-O -gdwarf-2 -dA" } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-assembler "DW_AT_const_value" } } */

extern void x () __attribute__((visibility ("hidden")));
void (* const f) () = x;
