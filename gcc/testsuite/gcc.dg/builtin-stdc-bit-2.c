/* { dg-do compile } */
/* { dg-options "" } */

void
foo (void)
{
  typedef int V __attribute__ ((vector_size (4 * sizeof (int))));
  struct S { int s; };
  enum E { E0, E1 };
  __builtin_stdc_leading_zeros (0.0f);	/* { dg-error "'__builtin_stdc_leading_zeros' operand not an integral type" } */
  __builtin_stdc_leading_zeros (0.0);	/* { dg-error "'__builtin_stdc_leading_zeros' operand not an integral type" } */
  __builtin_stdc_leading_zeros (0.0L);	/* { dg-error "'__builtin_stdc_leading_zeros' operand not an integral type" } */
  __builtin_stdc_leading_zeros ((V) {});	/* { dg-error "'__builtin_stdc_leading_zeros' operand not an integral type" } */
  __builtin_stdc_leading_zeros ((struct S) { 0 });	/* { dg-error "'__builtin_stdc_leading_zeros' operand not an integral type" } */
  __builtin_stdc_leading_zeros ();		/* { dg-error "wrong number of arguments to '__builtin_stdc_leading_zeros'" } */
  __builtin_stdc_leading_zeros (0U, 0U);	/* { dg-error "wrong number of arguments to '__builtin_stdc_leading_zeros'" } */
  __builtin_stdc_leading_zeros ((_Bool) 0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_leading_zeros' has boolean type" } */
  __builtin_stdc_leading_zeros ((enum E) E0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_leading_zeros' has enumerated type" } */
  __builtin_stdc_leading_zeros (0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_leading_zeros' has signed type" } */
  __builtin_stdc_leading_ones (0.0f);	/* { dg-error "'__builtin_stdc_leading_ones' operand not an integral type" } */
  __builtin_stdc_leading_ones (0.0);	/* { dg-error "'__builtin_stdc_leading_ones' operand not an integral type" } */
  __builtin_stdc_leading_ones (0.0L);	/* { dg-error "'__builtin_stdc_leading_ones' operand not an integral type" } */
  __builtin_stdc_leading_ones ((V) {});	/* { dg-error "'__builtin_stdc_leading_ones' operand not an integral type" } */
  __builtin_stdc_leading_ones ((struct S) { 0 });	/* { dg-error "'__builtin_stdc_leading_ones' operand not an integral type" } */
  __builtin_stdc_leading_ones ();		/* { dg-error "wrong number of arguments to '__builtin_stdc_leading_ones'" } */
  __builtin_stdc_leading_ones (0U, 0U);	/* { dg-error "wrong number of arguments to '__builtin_stdc_leading_ones'" } */
  __builtin_stdc_leading_ones ((_Bool) 0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_leading_ones' has boolean type" } */
  __builtin_stdc_leading_ones ((enum E) E0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_leading_ones' has enumerated type" } */
  __builtin_stdc_leading_ones (0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_leading_ones' has signed type" } */
  __builtin_stdc_trailing_zeros (0.0f);	/* { dg-error "'__builtin_stdc_trailing_zeros' operand not an integral type" } */
  __builtin_stdc_trailing_zeros (0.0);	/* { dg-error "'__builtin_stdc_trailing_zeros' operand not an integral type" } */
  __builtin_stdc_trailing_zeros (0.0L);	/* { dg-error "'__builtin_stdc_trailing_zeros' operand not an integral type" } */
  __builtin_stdc_trailing_zeros ((V) {});	/* { dg-error "'__builtin_stdc_trailing_zeros' operand not an integral type" } */
  __builtin_stdc_trailing_zeros ((struct S) { 0 });	/* { dg-error "'__builtin_stdc_trailing_zeros' operand not an integral type" } */
  __builtin_stdc_trailing_zeros ();		/* { dg-error "wrong number of arguments to '__builtin_stdc_trailing_zeros'" } */
  __builtin_stdc_trailing_zeros (0U, 0U);	/* { dg-error "wrong number of arguments to '__builtin_stdc_trailing_zeros'" } */
  __builtin_stdc_trailing_zeros ((_Bool) 0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_trailing_zeros' has boolean type" } */
  __builtin_stdc_trailing_zeros ((enum E) E0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_trailing_zeros' has enumerated type" } */
  __builtin_stdc_trailing_zeros (0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_trailing_zeros' has signed type" } */
  __builtin_stdc_trailing_ones (0.0f);	/* { dg-error "'__builtin_stdc_trailing_ones' operand not an integral type" } */
  __builtin_stdc_trailing_ones (0.0);	/* { dg-error "'__builtin_stdc_trailing_ones' operand not an integral type" } */
  __builtin_stdc_trailing_ones (0.0L);	/* { dg-error "'__builtin_stdc_trailing_ones' operand not an integral type" } */
  __builtin_stdc_trailing_ones ((V) {});	/* { dg-error "'__builtin_stdc_trailing_ones' operand not an integral type" } */
  __builtin_stdc_trailing_ones ((struct S) { 0 });	/* { dg-error "'__builtin_stdc_trailing_ones' operand not an integral type" } */
  __builtin_stdc_trailing_ones ();		/* { dg-error "wrong number of arguments to '__builtin_stdc_trailing_ones'" } */
  __builtin_stdc_trailing_ones (0U, 0U);	/* { dg-error "wrong number of arguments to '__builtin_stdc_trailing_ones'" } */
  __builtin_stdc_trailing_ones ((_Bool) 0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_trailing_ones' has boolean type" } */
  __builtin_stdc_trailing_ones ((enum E) E0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_trailing_ones' has enumerated type" } */
  __builtin_stdc_trailing_ones (0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_trailing_ones' has signed type" } */
  __builtin_stdc_first_leading_zero (0.0f);	/* { dg-error "'__builtin_stdc_first_leading_zero' operand not an integral type" } */
  __builtin_stdc_first_leading_zero (0.0);	/* { dg-error "'__builtin_stdc_first_leading_zero' operand not an integral type" } */
  __builtin_stdc_first_leading_zero (0.0L);	/* { dg-error "'__builtin_stdc_first_leading_zero' operand not an integral type" } */
  __builtin_stdc_first_leading_zero ((V) {});	/* { dg-error "'__builtin_stdc_first_leading_zero' operand not an integral type" } */
  __builtin_stdc_first_leading_zero ((struct S) { 0 });	/* { dg-error "'__builtin_stdc_first_leading_zero' operand not an integral type" } */
  __builtin_stdc_first_leading_zero ();		/* { dg-error "wrong number of arguments to '__builtin_stdc_first_leading_zero'" } */
  __builtin_stdc_first_leading_zero (0U, 0U);	/* { dg-error "wrong number of arguments to '__builtin_stdc_first_leading_zero'" } */
  __builtin_stdc_first_leading_zero ((_Bool) 0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_first_leading_zero' has boolean type" } */
  __builtin_stdc_first_leading_zero ((enum E) E0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_first_leading_zero' has enumerated type" } */
  __builtin_stdc_first_leading_zero (0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_first_leading_zero' has signed type" } */
  __builtin_stdc_first_leading_one (0.0f);	/* { dg-error "'__builtin_stdc_first_leading_one' operand not an integral type" } */
  __builtin_stdc_first_leading_one (0.0);	/* { dg-error "'__builtin_stdc_first_leading_one' operand not an integral type" } */
  __builtin_stdc_first_leading_one (0.0L);	/* { dg-error "'__builtin_stdc_first_leading_one' operand not an integral type" } */
  __builtin_stdc_first_leading_one ((V) {});	/* { dg-error "'__builtin_stdc_first_leading_one' operand not an integral type" } */
  __builtin_stdc_first_leading_one ((struct S) { 0 });	/* { dg-error "'__builtin_stdc_first_leading_one' operand not an integral type" } */
  __builtin_stdc_first_leading_one ();		/* { dg-error "wrong number of arguments to '__builtin_stdc_first_leading_one'" } */
  __builtin_stdc_first_leading_one (0U, 0U);	/* { dg-error "wrong number of arguments to '__builtin_stdc_first_leading_one'" } */
  __builtin_stdc_first_leading_one ((_Bool) 0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_first_leading_one' has boolean type" } */
  __builtin_stdc_first_leading_one ((enum E) E0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_first_leading_one' has enumerated type" } */
  __builtin_stdc_first_leading_one (0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_first_leading_one' has signed type" } */
  __builtin_stdc_first_trailing_zero (0.0f);	/* { dg-error "'__builtin_stdc_first_trailing_zero' operand not an integral type" } */
  __builtin_stdc_first_trailing_zero (0.0);	/* { dg-error "'__builtin_stdc_first_trailing_zero' operand not an integral type" } */
  __builtin_stdc_first_trailing_zero (0.0L);	/* { dg-error "'__builtin_stdc_first_trailing_zero' operand not an integral type" } */
  __builtin_stdc_first_trailing_zero ((V) {});	/* { dg-error "'__builtin_stdc_first_trailing_zero' operand not an integral type" } */
  __builtin_stdc_first_trailing_zero ((struct S) { 0 });	/* { dg-error "'__builtin_stdc_first_trailing_zero' operand not an integral type" } */
  __builtin_stdc_first_trailing_zero ();		/* { dg-error "wrong number of arguments to '__builtin_stdc_first_trailing_zero'" } */
  __builtin_stdc_first_trailing_zero (0U, 0U);	/* { dg-error "wrong number of arguments to '__builtin_stdc_first_trailing_zero'" } */
  __builtin_stdc_first_trailing_zero ((_Bool) 0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_first_trailing_zero' has boolean type" } */
  __builtin_stdc_first_trailing_zero ((enum E) E0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_first_trailing_zero' has enumerated type" } */
  __builtin_stdc_first_trailing_zero (0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_first_trailing_zero' has signed type" } */
  __builtin_stdc_first_trailing_one (0.0f);	/* { dg-error "'__builtin_stdc_first_trailing_one' operand not an integral type" } */
  __builtin_stdc_first_trailing_one (0.0);	/* { dg-error "'__builtin_stdc_first_trailing_one' operand not an integral type" } */
  __builtin_stdc_first_trailing_one (0.0L);	/* { dg-error "'__builtin_stdc_first_trailing_one' operand not an integral type" } */
  __builtin_stdc_first_trailing_one ((V) {});	/* { dg-error "'__builtin_stdc_first_trailing_one' operand not an integral type" } */
  __builtin_stdc_first_trailing_one ((struct S) { 0 });	/* { dg-error "'__builtin_stdc_first_trailing_one' operand not an integral type" } */
  __builtin_stdc_first_trailing_one ();		/* { dg-error "wrong number of arguments to '__builtin_stdc_first_trailing_one'" } */
  __builtin_stdc_first_trailing_one (0U, 0U);	/* { dg-error "wrong number of arguments to '__builtin_stdc_first_trailing_one'" } */
  __builtin_stdc_first_trailing_one ((_Bool) 0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_first_trailing_one' has boolean type" } */
  __builtin_stdc_first_trailing_one ((enum E) E0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_first_trailing_one' has enumerated type" } */
  __builtin_stdc_first_trailing_one (0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_first_trailing_one' has signed type" } */
  __builtin_stdc_count_zeros (0.0f);	/* { dg-error "'__builtin_stdc_count_zeros' operand not an integral type" } */
  __builtin_stdc_count_zeros (0.0);	/* { dg-error "'__builtin_stdc_count_zeros' operand not an integral type" } */
  __builtin_stdc_count_zeros (0.0L);	/* { dg-error "'__builtin_stdc_count_zeros' operand not an integral type" } */
  __builtin_stdc_count_zeros ((V) {});	/* { dg-error "'__builtin_stdc_count_zeros' operand not an integral type" } */
  __builtin_stdc_count_zeros ((struct S) { 0 });	/* { dg-error "'__builtin_stdc_count_zeros' operand not an integral type" } */
  __builtin_stdc_count_zeros ();		/* { dg-error "wrong number of arguments to '__builtin_stdc_count_zeros'" } */
  __builtin_stdc_count_zeros (0U, 0U);	/* { dg-error "wrong number of arguments to '__builtin_stdc_count_zeros'" } */
  __builtin_stdc_count_zeros ((_Bool) 0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_count_zeros' has boolean type" } */
  __builtin_stdc_count_zeros ((enum E) E0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_count_zeros' has enumerated type" } */
  __builtin_stdc_count_zeros (0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_count_zeros' has signed type" } */
  __builtin_stdc_count_ones (0.0f);	/* { dg-error "'__builtin_stdc_count_ones' operand not an integral type" } */
  __builtin_stdc_count_ones (0.0);	/* { dg-error "'__builtin_stdc_count_ones' operand not an integral type" } */
  __builtin_stdc_count_ones (0.0L);	/* { dg-error "'__builtin_stdc_count_ones' operand not an integral type" } */
  __builtin_stdc_count_ones ((V) {});	/* { dg-error "'__builtin_stdc_count_ones' operand not an integral type" } */
  __builtin_stdc_count_ones ((struct S) { 0 });	/* { dg-error "'__builtin_stdc_count_ones' operand not an integral type" } */
  __builtin_stdc_count_ones ();		/* { dg-error "wrong number of arguments to '__builtin_stdc_count_ones'" } */
  __builtin_stdc_count_ones (0U, 0U);	/* { dg-error "wrong number of arguments to '__builtin_stdc_count_ones'" } */
  __builtin_stdc_count_ones ((_Bool) 0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_count_ones' has boolean type" } */
  __builtin_stdc_count_ones ((enum E) E0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_count_ones' has enumerated type" } */
  __builtin_stdc_count_ones (0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_count_ones' has signed type" } */
  __builtin_stdc_has_single_bit (0.0f);	/* { dg-error "'__builtin_stdc_has_single_bit' operand not an integral type" } */
  __builtin_stdc_has_single_bit (0.0);	/* { dg-error "'__builtin_stdc_has_single_bit' operand not an integral type" } */
  __builtin_stdc_has_single_bit (0.0L);	/* { dg-error "'__builtin_stdc_has_single_bit' operand not an integral type" } */
  __builtin_stdc_has_single_bit ((V) {});	/* { dg-error "'__builtin_stdc_has_single_bit' operand not an integral type" } */
  __builtin_stdc_has_single_bit ((struct S) { 0 });	/* { dg-error "'__builtin_stdc_has_single_bit' operand not an integral type" } */
  __builtin_stdc_has_single_bit ();		/* { dg-error "wrong number of arguments to '__builtin_stdc_has_single_bit'" } */
  __builtin_stdc_has_single_bit (0U, 0U);	/* { dg-error "wrong number of arguments to '__builtin_stdc_has_single_bit'" } */
  __builtin_stdc_has_single_bit ((_Bool) 0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_has_single_bit' has boolean type" } */
  __builtin_stdc_has_single_bit ((enum E) E0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_has_single_bit' has enumerated type" } */
  __builtin_stdc_has_single_bit (0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_has_single_bit' has signed type" } */
  __builtin_stdc_bit_width (0.0f);	/* { dg-error "'__builtin_stdc_bit_width' operand not an integral type" } */
  __builtin_stdc_bit_width (0.0);	/* { dg-error "'__builtin_stdc_bit_width' operand not an integral type" } */
  __builtin_stdc_bit_width (0.0L);	/* { dg-error "'__builtin_stdc_bit_width' operand not an integral type" } */
  __builtin_stdc_bit_width ((V) {});	/* { dg-error "'__builtin_stdc_bit_width' operand not an integral type" } */
  __builtin_stdc_bit_width ((struct S) { 0 });	/* { dg-error "'__builtin_stdc_bit_width' operand not an integral type" } */
  __builtin_stdc_bit_width ();		/* { dg-error "wrong number of arguments to '__builtin_stdc_bit_width'" } */
  __builtin_stdc_bit_width (0U, 0U);	/* { dg-error "wrong number of arguments to '__builtin_stdc_bit_width'" } */
  __builtin_stdc_bit_width ((_Bool) 0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_bit_width' has boolean type" } */
  __builtin_stdc_bit_width ((enum E) E0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_bit_width' has enumerated type" } */
  __builtin_stdc_bit_width (0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_bit_width' has signed type" } */
  __builtin_stdc_bit_floor (0.0f);	/* { dg-error "'__builtin_stdc_bit_floor' operand not an integral type" } */
  __builtin_stdc_bit_floor (0.0);	/* { dg-error "'__builtin_stdc_bit_floor' operand not an integral type" } */
  __builtin_stdc_bit_floor (0.0L);	/* { dg-error "'__builtin_stdc_bit_floor' operand not an integral type" } */
  __builtin_stdc_bit_floor ((V) {});	/* { dg-error "'__builtin_stdc_bit_floor' operand not an integral type" } */
  __builtin_stdc_bit_floor ((struct S) { 0 });	/* { dg-error "'__builtin_stdc_bit_floor' operand not an integral type" } */
  __builtin_stdc_bit_floor ();		/* { dg-error "wrong number of arguments to '__builtin_stdc_bit_floor'" } */
  __builtin_stdc_bit_floor (0U, 0U);	/* { dg-error "wrong number of arguments to '__builtin_stdc_bit_floor'" } */
  __builtin_stdc_bit_floor ((_Bool) 0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_bit_floor' has boolean type" } */
  __builtin_stdc_bit_floor ((enum E) E0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_bit_floor' has enumerated type" } */
  __builtin_stdc_bit_floor (0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_bit_floor' has signed type" } */
  __builtin_stdc_bit_ceil (0.0f);	/* { dg-error "'__builtin_stdc_bit_ceil' operand not an integral type" } */
  __builtin_stdc_bit_ceil (0.0);	/* { dg-error "'__builtin_stdc_bit_ceil' operand not an integral type" } */
  __builtin_stdc_bit_ceil (0.0L);	/* { dg-error "'__builtin_stdc_bit_ceil' operand not an integral type" } */
  __builtin_stdc_bit_ceil ((V) {});	/* { dg-error "'__builtin_stdc_bit_ceil' operand not an integral type" } */
  __builtin_stdc_bit_ceil ((struct S) { 0 });	/* { dg-error "'__builtin_stdc_bit_ceil' operand not an integral type" } */
  __builtin_stdc_bit_ceil ();		/* { dg-error "wrong number of arguments to '__builtin_stdc_bit_ceil'" } */
  __builtin_stdc_bit_ceil (0U, 0U);	/* { dg-error "wrong number of arguments to '__builtin_stdc_bit_ceil'" } */
  __builtin_stdc_bit_ceil ((_Bool) 0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_bit_ceil' has boolean type" } */
  __builtin_stdc_bit_ceil ((enum E) E0);	/* { dg-error "argument 1 in call to function '__builtin_stdc_bit_ceil' has enumerated type" } */
  __builtin_stdc_bit_ceil (0);		/* { dg-error "argument 1 in call to function '__builtin_stdc_bit_ceil' has signed type" } */
}
