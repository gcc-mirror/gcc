/* { dg-do compile } */
/* { dg-options "-Ofast -march=rv64gc -mabi=lp64d" { target rv64 } } */
/* { dg-options "-Ofast -march=rv32gc -mabi=ilp32" { target rv32 } } */

_Float16 test__Float16_post_inc()
{
    _Atomic _Float16 n;
    return n++;
}
