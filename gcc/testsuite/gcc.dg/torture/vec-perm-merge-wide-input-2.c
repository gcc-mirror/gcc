/* forwprop must not treat permutes of different widths as an identity.  */
/* { dg-do run } */
/* { dg-additional-options "-fgimple -Wno-psabi" } */

typedef float v8sf __attribute__((vector_size(32)));
typedef float v4sf __attribute__((vector_size(16)));
typedef float v2sf __attribute__((vector_size(8)));
typedef int v4si __attribute__((vector_size(16)));
typedef int v2si __attribute__((vector_size(8)));
typedef short v4hi __attribute__((vector_size(8)));
typedef short v2hi __attribute__((vector_size(4)));

v8sf fa, fb;
v2sf fr;
v4hi ha, hb;
v2hi hr;

/* The composed mask { 3, 4 } has fewer elements than the inner one.  */
void __attribute__((noipa)) __GIMPLE() f1 ()
{
  v8sf a_; v8sf b_; v4sf c_; v2sf d_;
  a_ = fa; b_ = fb;
  c_ = __VEC_PERM (a_, b_, _Literal (v4si) { 4, 6, 12, 3 });
  d_ = __VEC_PERM (c_, c_, _Literal (v2si) { 6, 0 });
  fr = d_;
}

/* { 1, 0 } of { 7, 6 } looks like an identity on b if the indices are
   reduced by the length of c rather than of b.  */
void __attribute__((noipa)) __GIMPLE() f2 ()
{
  v4hi a_; v4hi b_; v2hi c_; v2hi d_;
  a_ = ha; b_ = hb;
  c_ = __VEC_PERM (a_, b_, _Literal (v2hi) { 7, 6 });
  d_ = __VEC_PERM (c_, c_, _Literal (v2hi) { 1, 2 });
  hr = d_;
}

int
main ()
{
  for (int i = 0; i < 8; i++)
    {
      fa[i] = i + 1;
      fb[i] = i + 101;
    }
  for (int i = 0; i < 4; i++)
    {
      ha[i] = i + 1;
      hb[i] = i + 101;
    }

  f1 ();
  /* c = { a4, a6, b4, a3 }, d = { c2, c0 }.  */
  if (fr[0] != 105 || fr[1] != 5)
    __builtin_abort ();

  f2 ();
  /* c = { b3, b2 }, d = { c1, c0 }.  */
  if (hr[0] != 103 || hr[1] != 104)
    __builtin_abort ();

  return 0;
}
