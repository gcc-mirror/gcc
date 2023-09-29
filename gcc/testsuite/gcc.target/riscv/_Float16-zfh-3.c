/* { dg-do compile } */
/* { dg-options "-march=rv64if_zfh -mabi=lp64f -O" } */

int foo1 (_Float16 a, _Float16 b)
{
    /* { dg-final { scan-assembler-times {\mfgt\.h\M} 1 } } */
    return a > b;
}
