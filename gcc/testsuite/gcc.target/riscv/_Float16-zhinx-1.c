/* { dg-do compile } */
/* { dg-options "-march=rv64i_zhinx -mabi=lp64 -O" } */

_Float16 foo1 (_Float16 a, _Float16 b)
{
    return b;
}

/* { dg-final { scan-assembler-not {\mfmv\.h\M} } } */
/* { dg-final { scan-assembler-times {\mmv\M} 1 } } */
