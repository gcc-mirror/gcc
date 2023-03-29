/* { dg-do compile } */
/* { dg-options "-march=rv64if_zfhmin -mabi=lp64f -O" } */

int foo1 (_Float16 a, _Float16 b)
{
    /* { dg-final { scan-assembler-not "fgt.h" } } */
    /* { dg-final { scan-assembler-not "fgt.s	fa" } } */
    /* { dg-final { scan-assembler-times "fgt.s	a" 1 } } */
    return a > b;
}
