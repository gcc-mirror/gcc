/* BIT_FIELD_REF of a VEC_PERM_EXPR with inputs narrower than the result.  */
/* { dg-do run } */
/* { dg-additional-options "-fgimple -Wno-psabi" } */

typedef short v8hi __attribute__((vector_size(16)));
typedef short v4hi __attribute__((vector_size(8)));
typedef short v2hi __attribute__((vector_size(4)));

v4hi a, b;
short s;
v2hi r;

/* One element.  */
void __attribute__((noipa)) __GIMPLE() f1 ()
{
  v4hi a_; v4hi b_; v8hi c_; short e_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v8hi) { 0, 5, 2, 3, 4, 5, 6, 7 });
  e_ = __BIT_FIELD_REF <short> (c_, 16, 16);
  s = e_;
}

/* A subvector.  */
void __attribute__((noipa)) __GIMPLE() f2 ()
{
  v4hi a_; v4hi b_; v8hi c_; v2hi e_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v8hi) { 0, 1, 6, 7, 4, 5, 2, 3 });
  e_ = __BIT_FIELD_REF <v2hi> (c_, 32, 32);
  r = e_;
}

int
main ()
{
  for (int i = 0; i < 4; i++)
    {
      a[i] = i + 1;
      b[i] = i + 101;
    }

  f1 ();
  /* c[1] = b1.  */
  if (s != 102)
    __builtin_abort ();

  f2 ();
  /* c[2] = b2, c[3] = b3.  */
  if (r[0] != 103 || r[1] != 104)
    __builtin_abort ();

  return 0;
}
