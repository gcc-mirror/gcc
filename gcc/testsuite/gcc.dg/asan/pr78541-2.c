/* PR sanitizer/78560 */
/* { dg-do compile } */

void __quadmath_mpn_extract_flt128 (long *fp_input);

int fn1 ()
{
  long fp_input[1];
  int hack_digit () { __quadmath_mpn_extract_flt128 (fp_input); }
}
