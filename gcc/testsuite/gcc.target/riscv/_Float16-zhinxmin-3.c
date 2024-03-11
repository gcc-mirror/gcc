/* { dg-do compile } */
/* { dg-options "-march=rv64i_zhinxmin -mabi=lp64 -O" } */

int foo1 (_Float16 a, _Float16 b)
{
    /* { dg-final { scan-assembler-not {\mfgt\.h\M} } } */
    /* { dg-final { scan-assembler-not "fgt.s	fa" } } */
    /* { dg-final { scan-assembler-times "fgt.s	a" 1 } } */
    return a > b;
}
