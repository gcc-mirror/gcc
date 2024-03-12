/* { dg-do compile } */
/* { dg-options "-march=rv64if_zfhmin -mabi=lp64f -O" } */

int foo1 (_Float16 a, _Float16 b)
{
    /* { dg-final { scan-assembler-not {\mfgt\.h\M} } } */
    /* { dg-final { scan-assembler-times {\mfgt\.s\M} 1 } } */
    return a > b;
}
