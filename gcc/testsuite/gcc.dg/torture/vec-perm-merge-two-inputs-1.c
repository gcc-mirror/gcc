/* Merging VEC_PERM_EXPRs whose combined inputs are only a and b, and
   permutes reading lanes of an uninitialized vector.  */
/* { dg-do run } */
/* { dg-additional-options "-fgimple -Wno-psabi" } */

typedef short v4hi __attribute__((vector_size(8)));
typedef short v8hi __attribute__((vector_size(16)));

v4hi a4, b4, r4;
v8hi a8, b8, r8;

/* d = <a, c>, c = <a, b>: d uses both a and b.  */
void __attribute__((noipa)) __GIMPLE() f1 ()
{
  v4hi a_; v4hi b_; v4hi c_; v4hi d_;
  a_ = a4; b_ = b4;
  c_ = __VEC_PERM (a_, b_, _Literal (v4hi) { 7, 0, 5, 2 });
  d_ = __VEC_PERM (a_, c_, _Literal (v4hi) { 4, 3, 5, 1 });
  r4 = d_;
}

/* d = <b, c>.  */
void __attribute__((noipa)) __GIMPLE() f2 ()
{
  v4hi a_; v4hi b_; v4hi c_; v4hi d_;
  a_ = a4; b_ = b4;
  c_ = __VEC_PERM (a_, b_, _Literal (v4hi) { 7, 0, 5, 2 });
  d_ = __VEC_PERM (b_, c_, _Literal (v4hi) { 4, 3, 5, 1 });
  r4 = d_;
}

/* d = <c, a>.  */
void __attribute__((noipa)) __GIMPLE() f3 ()
{
  v8hi a_; v8hi b_; v8hi c_; v8hi d_;
  a_ = a8; b_ = b8;
  c_ = __VEC_PERM (a_, b_, _Literal (v8hi) { 15, 0, 9, 3, 12, 6, 8, 1 });
  d_ = __VEC_PERM (c_, a_, _Literal (v8hi) { 0, 13, 2, 8, 4, 11, 6, 1 });
  r8 = d_;
}

/* d = <c, b>.  */
void __attribute__((noipa)) __GIMPLE() f4 ()
{
  v8hi a_; v8hi b_; v8hi c_; v8hi d_;
  a_ = a8; b_ = b8;
  c_ = __VEC_PERM (a_, b_, _Literal (v8hi) { 15, 0, 9, 3, 12, 6, 8, 1 });
  d_ = __VEC_PERM (c_, b_, _Literal (v8hi) { 0, 13, 2, 8, 4, 11, 6, 1 });
  r8 = d_;
}

/* Building a vector up from an uninitialized one.  c takes
   lanes from the uninitialized u that later permutes overwrite.  */
void __attribute__((noipa)) __GIMPLE() f5 ()
{
  v4hi a_; v4hi b_; v4hi u_; v4hi c_; v4hi d_; v4hi e_; v4hi f_;
  a_ = a4; b_ = b4;
  c_ = __VEC_PERM (a_, u_, _Literal (v4hi) { 2, 5, 6, 7 });
  d_ = __VEC_PERM (c_, a_, _Literal (v4hi) { 0, 7, 2, 3 });
  e_ = __VEC_PERM (d_, b_, _Literal (v4hi) { 0, 1, 6, 3 });
  f_ = __VEC_PERM (e_, b_, _Literal (v4hi) { 0, 1, 2, 7 });
  r4 = f_;
}

/* As f5 with the uninitialized vector first.  */
void __attribute__((noipa)) __GIMPLE() f6 ()
{
  v4hi a_; v4hi b_; v4hi u_; v4hi c_; v4hi d_; v4hi e_; v4hi f_;
  a_ = a4; b_ = b4;
  c_ = __VEC_PERM (u_, b_, _Literal (v4hi) { 0, 5, 2, 3 });
  d_ = __VEC_PERM (c_, b_, _Literal (v4hi) { 7, 1, 2, 3 });
  e_ = __VEC_PERM (d_, a_, _Literal (v4hi) { 0, 1, 4, 3 });
  f_ = __VEC_PERM (e_, a_, _Literal (v4hi) { 0, 1, 2, 6 });
  r4 = f_;
}

/* d = <c, c>: x is c itself rather than one of a and b.  */
void __attribute__((noipa)) __GIMPLE() f7 ()
{
  v4hi a_; v4hi b_; v4hi c_; v4hi d_;
  a_ = a4; b_ = b4;
  c_ = __VEC_PERM (a_, b_, _Literal (v4hi) { 6, 2, 2, 1 });
  d_ = __VEC_PERM (c_, c_, _Literal (v4hi) { 4, 3, 5, 1 });
  r4 = d_;
}

/* Reference VEC_PERM_EXPR: OUT[i] = concat (X, Y)[SEL[i] % (2 * N)].  */
static void
perm (short *out, const short *x, const short *y, int n, const int *sel)
{
  short tmp[8];
  for (int i = 0; i < n; i++)
    {
      int j = sel[i] % (2 * n);
      tmp[i] = j < n ? x[j] : y[j - n];
    }
  for (int i = 0; i < n; i++)
    out[i] = tmp[i];
}

static void
check (const short *got, const short *want, int n)
{
  for (int i = 0; i < n; i++)
    if (got[i] != want[i])
      __builtin_abort ();
}

int
main ()
{
  short c[8], d[8];
  for (int i = 0; i < 4; i++)
    {
      a4[i] = i + 1;
      b4[i] = i + 101;
    }
  for (int i = 0; i < 8; i++)
    {
      a8[i] = i + 1;
      b8[i] = i + 101;
    }

  {
    static const int s0[4] = { 7, 0, 5, 2 }, s1[4] = { 4, 3, 5, 1 };
    perm (c, (short *) &a4, (short *) &b4, 4, s0);
    perm (d, (short *) &a4, c, 4, s1);
    f1 ();
    check ((short *) &r4, d, 4);
    perm (d, (short *) &b4, c, 4, s1);
    f2 ();
    check ((short *) &r4, d, 4);
  }
  {
    static const int s0[4] = { 6, 2, 2, 1 }, s1[4] = { 4, 3, 5, 1 };
    perm (c, (short *) &a4, (short *) &b4, 4, s0);
    perm (d, c, c, 4, s1);
    f7 ();
    check ((short *) &r4, d, 4);
  }
  {
    static const int s0[8] = { 15, 0, 9, 3, 12, 6, 8, 1 };
    static const int s1[8] = { 0, 13, 2, 8, 4, 11, 6, 1 };
    perm (c, (short *) &a8, (short *) &b8, 8, s0);
    perm (d, c, (short *) &a8, 8, s1);
    f3 ();
    check ((short *) &r8, d, 8);
    perm (d, c, (short *) &b8, 8, s1);
    f4 ();
    check ((short *) &r8, d, 8);
  }

  /* f5: { a2, a3, b2, b3 }.  */
  f5 ();
  if (r4[0] != 3 || r4[1] != 4 || r4[2] != 103 || r4[3] != 104)
    __builtin_abort ();

  /* f6: { b3, b1, a0, a2 }.  */
  f6 ();
  if (r4[0] != 104 || r4[1] != 102 || r4[2] != 1 || r4[3] != 3)
    __builtin_abort ();

  return 0;
}
