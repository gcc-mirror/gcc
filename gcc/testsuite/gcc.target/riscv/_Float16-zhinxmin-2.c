/* { dg-do compile } */
/* { dg-options "-march=rv64i_zhinxmin -mabi=lp64 -O" } */

_Float16 foo1 (_Float16 a, _Float16 b)
{
    /* { dg-final { scan-assembler-not "fadd.h" } } */
    /* { dg-final { scan-assembler-not "fadd.s	fa" } } */
    /* { dg-final { scan-assembler-times "fadd.s	a" 1 } } */
    return a + b;
}
