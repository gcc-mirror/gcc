/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xtheadbb" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xtheadbb" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" "-Oz" } } */

signed long extqi(signed char i)
{
    return --i;
}

/* { dg-final { scan-assembler "th.ext\ta\[0-9\]+,a\[0-9\]+,7,0" } } */
/* { dg-final { scan-assembler-not "th.ext\ta\[0-9\]+,a\[0-9\]+,15,0" } } */
