/* { dg-do compile } */
/* { dg-options "-march=rv64i_zhinxmin -mabi=lp64 -O" } */

_Float16 foo1 (_Float16 a, _Float16 b)
{
    /* { dg-final { scan-assembler-not {\mfmv\.h\M} } } */
    /* { dg-final { scan-assembler-not {\mfmv\.s\M} } } */
    /* { dg-final { scan-assembler-times "mv\ta0" 1 } } */
    return b;
}
