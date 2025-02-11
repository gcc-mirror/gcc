/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadvector -O2" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadvector -O2" { target { rv64 } } } */

void foo1_16 (void *p)
{
   __builtin_memset (p, 1, 16);
}
