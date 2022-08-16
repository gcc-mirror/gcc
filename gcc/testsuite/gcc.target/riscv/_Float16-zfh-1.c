/* { dg-do compile } */
/* { dg-options "-march=rv64if_zfh -mabi=lp64f -O" } */

_Float16 foo1 (_Float16 a, _Float16 b)
{
    /* { dg-final { scan-assembler-times "fmv.h" 1 } } */
    return b;
}
