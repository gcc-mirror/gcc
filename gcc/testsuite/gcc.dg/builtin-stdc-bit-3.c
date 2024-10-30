/* { dg-do compile } */
/* { dg-options "-funsigned-char" } */

void
foo (void)
{
  __builtin_stdc_leading_zeros ((char) 0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_leading_zeros' has 'char' type" } */
  __builtin_stdc_leading_ones ((char) 0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_leading_ones' has 'char' type" } */
  __builtin_stdc_trailing_zeros ((char) 0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_trailing_zeros' has 'char' type" } */
  __builtin_stdc_trailing_ones ((char) 0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_trailing_ones' has 'char' type" } */
  __builtin_stdc_first_leading_zero ((char) 0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_first_leading_zero' has 'char' type" } */
  __builtin_stdc_first_leading_one ((char) 0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_first_leading_one' has 'char' type" } */
  __builtin_stdc_first_trailing_zero ((char) 0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_first_trailing_zero' has 'char' type" } */
  __builtin_stdc_first_trailing_one ((char) 0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_first_trailing_one' has 'char' type" } */
  __builtin_stdc_count_zeros ((char) 0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_count_zeros' has 'char' type" } */
  __builtin_stdc_count_ones ((char) 0);			/* { dg-error "argument 1 in call to function '__builtin_stdc_count_ones' has 'char' type" } */
  __builtin_stdc_has_single_bit ((char) 0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_has_single_bit' has 'char' type" } */
  __builtin_stdc_bit_width ((char) 0);			/* { dg-error "argument 1 in call to function '__builtin_stdc_bit_width' has 'char' type" } */
  __builtin_stdc_bit_floor ((char) 0);			/* { dg-error "argument 1 in call to function '__builtin_stdc_bit_floor' has 'char' type" } */
  __builtin_stdc_bit_ceil ((char) 0);			/* { dg-error "argument 1 in call to function '__builtin_stdc_bit_ceil' has 'char' type" } */
}
