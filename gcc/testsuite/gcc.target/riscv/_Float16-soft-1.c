/* { dg-do compile } */
/* { dg-options "-march=rv64if -mabi=lp64f -O" } */

_Float16 test_soft_move (_Float16 a, _Float16 b)
{
    return b;
}

/* { dg-final { scan-assembler-not "fmv.h" } } */
