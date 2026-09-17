/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-Wno-psabi -fdump-tree-forwprop1-raw" } */

/* All six rules on a vector type.  The selects against zero are spelled with
   >, not <: a vector x < 0 is canonicalised to the sign mask
   x >> (precision - 1), and the rule that turns x & (x >> (precision - 1))
   into MIN <x, 0> is scalar, so that spelling forms no vector MIN_EXPR at
   all.  The scan is on forwprop1, which runs before the vector lowering that
   would split a MAX_EXPR the target cannot do.  The rules ask
   target_supports_op_p for the vector MIN_EXPR, hence the selector on the
   scans.  */

typedef int v4si __attribute__ ((vector_size (4 * sizeof (int))));

/* Y + MIN (X - Y, 0) -> MIN (X, Y).  */
v4si
add_min_vec (v4si a, v4si b)
{
  v4si t = a - b, z = { 0, 0, 0, 0 };
  v4si m = t > z;
  return b + ((z & m) | (t & ~m));
}

/* Y + MAX (X - Y, 0) -> MAX (X, Y).  */
v4si
add_max_vec (v4si a, v4si b)
{
  v4si t = a - b, z = { 0, 0, 0, 0 };
  v4si m = t > z;
  return b + ((t & m) | (z & ~m));
}

/* X - MIN (X - Y, 0) -> MAX (X, Y).  */
v4si
sub_min_vec (v4si a, v4si b)
{
  v4si t = a - b, z = { 0, 0, 0, 0 };
  v4si m = t > z;
  return a - ((z & m) | (t & ~m));
}

/* X - MAX (X - Y, 0) -> MIN (X, Y).  */
v4si
sub_max_vec (v4si a, v4si b)
{
  v4si t = a - b, z = { 0, 0, 0, 0 };
  v4si m = t > z;
  return a - ((t & m) | (z & ~m));
}

/* MIN (X + Y, Y) - Y -> MIN (X, 0).  */
v4si
clamp_lo_vec (v4si x, v4si y)
{
  v4si s = x + y;
  v4si m = s < y;
  return ((s & m) | (y & ~m)) - y;
}

/* MAX (X + Y, Y) - Y -> MAX (X, 0).  */
v4si
clamp_hi_vec (v4si x, v4si y)
{
  v4si s = x + y;
  v4si m = s > y;
  return ((s & m) | (y & ~m)) - y;
}

/* { dg-final { scan-tree-dump-not "<plus_expr," "forwprop1" { target { vect128 && { ! vect_no_int_min_max } } } } } */
/* { dg-final { scan-tree-dump-not "<minus_expr," "forwprop1" { target { vect128 && { ! vect_no_int_min_max } } } } } */
/* { dg-final { scan-tree-dump-times "<min_expr," 3 "forwprop1" { target { vect128 && { ! vect_no_int_min_max } } } } } */
/* { dg-final { scan-tree-dump-times "<max_expr," 3 "forwprop1" { target { vect128 && { ! vect_no_int_min_max } } } } } */
