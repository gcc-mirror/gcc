/* { dg-do compile } */

int f(long l, short *sp) {
  unsigned short us;
  for (; l; l -= 4, sp += 4)
    us += sp[1] + sp[3];
  return us;
}

/* ???  Both SVE and RVV refuse to do the { 1, 3 } permutation as two ld2
   or ld1 with odd extract plus lo/hi concat.  Instead they prefer ld4.  */
/* { dg-final { scan-tree-dump "vectorizing a reduction chain" "vect" { target { { vect_extract_even_odd && vect_int } && { ! vect_variable_length } } } } } */
