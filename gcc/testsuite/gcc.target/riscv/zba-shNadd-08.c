/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64 -O2" } */
/* { dg-final { scan-assembler "sh3add" } } */

long
test (long x, long y)
{
  return ((x | 0x1ff) << 3) + y;
}
