/* { dg-do compile { target bfin-*-* } } */
typedef long fract32;
main() {
  fract32 val_tmp;
  fract32 val1 = 0x7FFFFFFF;
  fract32 val2 = 0x40000000;
  val_tmp = __builtin_bfin_mult_fr1x32x32 (0x06666667, val1);
  val2 = __builtin_bfin_mult_fr1x32x32 (0x79999999, val2);
  val2 = __builtin_bfin_add_fr1x32 (val_tmp, val2);
}
