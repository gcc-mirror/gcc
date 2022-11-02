/* { dg-do compile } */
/* { dg-options "-march=rv64i_zhinx -mabi=lp64 -O" } */

int foo1 (_Float16 a, _Float16 b)
{
    /* { dg-final { scan-assembler-not "fgt.h	fa" } } */
    /* { dg-final { scan-assembler-times "fgt.h	a" 1 } } */
    return a > b;
}
