/* { dg-options "-O2 -fdump-ipa-modref"  } */
/* { dg-do compile } */
void
test (int *a, int size)
{
 __builtin_memset (a, 0, 321);
}
void
test2 (double x, double *y)
{
 __builtin_modf (x,y);
}
/* 321*8 */
/* { dg-final { scan-ipa-dump "Parm 0 param offset:0 offset:0 size:-1 max_size:2568" "modref" } } */
/* { dg-final { scan-ipa-dump "Parm 1 param offset:0 offset:0 size:-1 max_size:64" "modref" { target lp64 } } } */
/* { dg-final { scan-ipa-dump "Parm 1 param offset:0 offset:0 size:-1 max_size:32" "modref" { target ilp32 } } } */
