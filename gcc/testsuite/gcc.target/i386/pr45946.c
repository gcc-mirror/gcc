/* { dg-do compile } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-std=gnu99 -Os -fno-omit-frame-pointer" } */

void
__attribute__((noinline))
bar (_Decimal128, _Decimal128, _Decimal128, _Decimal128, _Decimal128,
     _Decimal128, _Decimal128, _Decimal128, _Decimal128);

void
foo (void)
{
  bar (0, 0, 0, 0, 0, 0, 0, 0, 0);
}
