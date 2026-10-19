/* VEC_PERM_EXPR merges with inputs wider than the result.  */
/* { dg-do run } */
/* { dg-additional-options "-fgimple -Wno-psabi" } */

typedef int v4si __attribute__((vector_size(16)));
typedef int v8si __attribute__((vector_size(32)));
typedef int v16si __attribute__((vector_size(64)));
typedef float v4sf __attribute__((vector_size(16)));
typedef float v8sf __attribute__((vector_size(32)));

v8si a8, b8, x8;
v16si a16, b16;
v4si r4;
v8si r8;
v4sf f4;

/* c = <a, b> v8si; d = <c, c> v4si.  */
void __attribute__((noipa)) __GIMPLE() fa1 ()
{
  v8si a_; v8si b_; v8si c_; v4si d_;
  a_ = a8; b_ = b8;
  c_ = __VEC_PERM (a_, b_, _Literal (v8si) { 8, 0, 9, 1, 10, 2, 11, 3 });
  d_ = __VEC_PERM (c_, c_, _Literal (v4si) { 1, 2, 3, 4 });
  r4 = d_;
}

/* c = <a, b> v8si from v16si; d = <c, c> v8si.  */
void __attribute__((noipa)) __GIMPLE() fa2 ()
{
  v16si a_; v16si b_; v8si c_; v8si d_;
  a_ = a16; b_ = b16;
  c_ = __VEC_PERM (a_, b_, _Literal (v8si) { 31, 0, 17, 14, 16, 15, 1, 30 });
  d_ = __VEC_PERM (c_, c_, _Literal (v8si) { 7, 6, 5, 4, 3, 2, 1, 0 });
  r8 = d_;
}

/* As fa1, through a VIEW_CONVERT_EXPR.  */
void __attribute__((noipa)) __GIMPLE() fa3 ()
{
  v8si a_; v8si b_; v8si c_; v8sf e_; v4sf d_;
  a_ = a8; b_ = b8;
  c_ = __VEC_PERM (a_, b_, _Literal (v8si) { 8, 0, 9, 1, 10, 2, 11, 3 });
  e_ = __VIEW_CONVERT <v8sf> (c_);
  d_ = __VEC_PERM (e_, e_, _Literal (v4si) { 7, 0, 5, 2 });
  f4 = d_;
}

/* c = <a, b> v8si; d = <x, c> v4si; only a used from c.  */
void __attribute__((noipa)) __GIMPLE() fb1 ()
{
  v8si a_; v8si b_; v8si x_; v8si c_; v4si d_;
  a_ = a8; b_ = b8; x_ = x8;
  c_ = __VEC_PERM (a_, b_, _Literal (v8si) { 7, 6, 5, 4, 8, 9, 10, 11 });
  d_ = __VEC_PERM (x_, c_, _Literal (v4si) { 0, 9, 7, 11 });
  r4 = d_;
}

/* c = <a, b> v8si from v16si; d = <x, c> v8si; only b used from c.  */
void __attribute__((noipa)) __GIMPLE() fb2 ()
{
  v16si a_; v16si b_; v8si x_; v8si c_; v8si d_;
  a_ = a16; b_ = b16; x_ = x8;
  c_ = __VEC_PERM (a_, b_, _Literal (v8si) { 31, 1, 17, 2, 16, 3, 20, 4 });
  d_ = __VEC_PERM (x_, c_, _Literal (v8si) { 0, 8, 1, 10, 2, 12, 3, 14 });
  r8 = d_;
}

/* c = <a, b> v8si; d = <c, x> v4si; only b used from c.  */
void __attribute__((noipa)) __GIMPLE() fc1 ()
{
  v8si a_; v8si b_; v8si x_; v8si c_; v4si d_;
  a_ = a8; b_ = b8; x_ = x8;
  c_ = __VEC_PERM (a_, b_, _Literal (v8si) { 0, 1, 2, 3, 15, 14, 13, 12 });
  d_ = __VEC_PERM (c_, x_, _Literal (v4si) { 4, 9, 6, 15 });
  r4 = d_;
}

/* c = <a, b> v8si from v16si; d = <c, x> v8si; only a used from c.  */
void __attribute__((noipa)) __GIMPLE() fc2 ()
{
  v16si a_; v16si b_; v8si x_; v8si c_; v8si d_;
  a_ = a16; b_ = b16; x_ = x8;
  c_ = __VEC_PERM (a_, b_, _Literal (v8si) { 15, 0, 13, 2, 11, 4, 9, 6 });
  d_ = __VEC_PERM (c_, x_, _Literal (v8si) { 0, 8, 2, 9, 4, 10, 6, 11 });
  r8 = d_;
}

/* Reference VEC_PERM_EXPR: OUT[i] = concat (X, Y)[SEL[i] % (2 * NX)].  */
static void
perm (int *out, const int *x, const int *y, int nx, const int *sel, int n)
{
  int tmp[16];
  for (int i = 0; i < n; i++)
    {
      int j = sel[i] % (2 * nx);
      tmp[i] = j < nx ? x[j] : y[j - nx];
    }
  for (int i = 0; i < n; i++)
    out[i] = tmp[i];
}

static int fails;

static void
check (const char *name, const int *got, const int *want, int n)
{
  for (int i = 0; i < n; i++)
    if (got[i] != want[i])
      {
	__builtin_printf ("%s: lane %d got %d want %d\n", name, i, got[i],
			  want[i]);
	fails++;
	return;
      }
}

int
main ()
{
  int c[8], d[8];
  for (int i = 0; i < 8; i++)
    {
      a8[i] = i + 1;
      b8[i] = i + 101;
      x8[i] = i + 201;
    }
  for (int i = 0; i < 16; i++)
    {
      a16[i] = i + 301;
      b16[i] = i + 401;
    }

  {
    static const int s0[8] = { 8, 0, 9, 1, 10, 2, 11, 3 };
    static const int s1[4] = { 1, 2, 3, 4 };
    perm (c, (int *) &a8, (int *) &b8, 8, s0, 8);
    perm (d, c, c, 8, s1, 4);
    fa1 ();
    check ("fa1", (int *) &r4, d, 4);
  }
  {
    static const int s0[8] = { 31, 0, 17, 14, 16, 15, 1, 30 };
    static const int s1[8] = { 7, 6, 5, 4, 3, 2, 1, 0 };
    perm (c, (int *) &a16, (int *) &b16, 16, s0, 8);
    perm (d, c, c, 8, s1, 8);
    fa2 ();
    check ("fa2", (int *) &r8, d, 8);
  }
  {
    static const int s0[8] = { 8, 0, 9, 1, 10, 2, 11, 3 };
    static const int s1[4] = { 7, 0, 5, 2 };
    perm (c, (int *) &a8, (int *) &b8, 8, s0, 8);
    perm (d, c, c, 8, s1, 4);
    fa3 ();
    check ("fa3", (int *) &f4, d, 4);
  }
  {
    static const int s0[8] = { 7, 6, 5, 4, 8, 9, 10, 11 };
    static const int s1[4] = { 0, 9, 7, 11 };
    perm (c, (int *) &a8, (int *) &b8, 8, s0, 8);
    perm (d, (int *) &x8, c, 8, s1, 4);
    fb1 ();
    check ("fb1", (int *) &r4, d, 4);
  }
  {
    static const int s0[8] = { 31, 1, 17, 2, 16, 3, 20, 4 };
    static const int s1[8] = { 0, 8, 1, 10, 2, 12, 3, 14 };
    perm (c, (int *) &a16, (int *) &b16, 16, s0, 8);
    perm (d, (int *) &x8, c, 8, s1, 8);
    fb2 ();
    check ("fb2", (int *) &r8, d, 8);
  }
  {
    static const int s0[8] = { 0, 1, 2, 3, 15, 14, 13, 12 };
    static const int s1[4] = { 4, 9, 6, 15 };
    perm (c, (int *) &a8, (int *) &b8, 8, s0, 8);
    perm (d, c, (int *) &x8, 8, s1, 4);
    fc1 ();
    check ("fc1", (int *) &r4, d, 4);
  }
  {
    static const int s0[8] = { 15, 0, 13, 2, 11, 4, 9, 6 };
    static const int s1[8] = { 0, 8, 2, 9, 4, 10, 6, 11 };
    perm (c, (int *) &a16, (int *) &b16, 16, s0, 8);
    perm (d, c, (int *) &x8, 8, s1, 8);
    fc2 ();
    check ("fc2", (int *) &r8, d, 8);
  }

  if (fails)
    __builtin_abort ();
  return 0;
}
