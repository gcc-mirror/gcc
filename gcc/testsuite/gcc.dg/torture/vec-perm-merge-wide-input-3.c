/* VEC_PERM_EXPR merges of c = <a, b> into d = <c, c> with c wider than, as
   wide as and narrower than a, and d wider than, as wide as and narrower
   than c.  */
/* { dg-do run } */
/* { dg-additional-options "-fgimple -Wno-psabi" } */

typedef int v2si __attribute__((vector_size(8)));
typedef int v4si __attribute__((vector_size(16)));
typedef int v8si __attribute__((vector_size(32)));
typedef int v16si __attribute__((vector_size(64)));
typedef int v32si __attribute__((vector_size(128)));

v8si a, b;
v2si r2;
v4si r4;
v8si r8;
v16si r16;
v32si r32;

/* c wider than a (v8si -> v16si), d wider than c (-> v32si).  */

void __attribute__((noipa)) __GIMPLE() f1 ()
{
  v8si a_; v8si b_; v16si c_; v32si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v16si) {
      14, 14, 14, 6, 5, 15, 5, 3, 14, 9, 4, 2, 1, 12, 14, 5 });
  d_ = __VEC_PERM (c_, c_, _Literal (v32si) {
      0, 4, 3, 2, 12, 15, 1, 29, 20, 28, 12, 14, 18, 31, 0, 5, 29, 17, 26, 5,
      16, 20, 14, 18, 1, 4, 6, 25, 6, 18, 24, 4 });
  r32 = d_;
}

void __attribute__((noipa)) __GIMPLE() f2 ()
{
  v8si a_; v8si b_; v16si c_; v32si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v16si) {
      0, 0, 6, 6, 1, 15, 12, 12, 13, 2, 6, 8, 10, 2, 9, 10 });
  d_ = __VEC_PERM (c_, c_, _Literal (v32si) {
      0, 26, 7, 8, 15, 6, 0, 3, 29, 31, 11, 12, 28, 12, 8, 26, 24, 7, 25, 26,
      13, 0, 17, 19, 1, 13, 11, 25, 6, 2, 9, 13 });
  r32 = d_;
}

/* c wider than a (v8si -> v16si), d as wide as c (-> v16si).  */

void __attribute__((noipa)) __GIMPLE() f3 ()
{
  v8si a_; v8si b_; v16si c_; v16si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v16si) {
      14, 8, 0, 10, 9, 12, 2, 2, 2, 6, 7, 0, 11, 11, 14, 4 });
  d_ = __VEC_PERM (c_, c_, _Literal (v16si) {
      30, 8, 24, 11, 9, 19, 14, 15, 12, 10, 12, 24, 30, 5, 26, 3 });
  r16 = d_;
}

void __attribute__((noipa)) __GIMPLE() f4 ()
{
  v8si a_; v8si b_; v16si c_; v16si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v16si) {
      3, 3, 1, 8, 7, 12, 8, 13, 15, 9, 5, 2, 4, 7, 15, 2 });
  d_ = __VEC_PERM (c_, c_, _Literal (v16si) {
      17, 13, 13, 1, 4, 17, 26, 28, 15, 3, 2, 11, 18, 23, 8, 5 });
  r16 = d_;
}

/* c wider than a (v8si -> v16si), d narrower than c (-> v8si).  */

void __attribute__((noipa)) __GIMPLE() f5 ()
{
  v8si a_; v8si b_; v16si c_; v8si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v16si) {
      11, 4, 14, 10, 4, 1, 0, 15, 11, 9, 1, 0, 2, 15, 2, 9 });
  d_ = __VEC_PERM (c_, c_, _Literal (v8si) { 20, 8, 4, 4, 28, 23, 2, 8 });
  r8 = d_;
}

void __attribute__((noipa)) __GIMPLE() f6 ()
{
  v8si a_; v8si b_; v16si c_; v8si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v16si) {
      10, 11, 2, 15, 2, 13, 0, 15, 0, 12, 12, 0, 2, 2, 2, 3 });
  d_ = __VEC_PERM (c_, c_, _Literal (v8si) { 16, 26, 21, 24, 29, 28, 29, 5 });
  r8 = d_;
}

/* c as wide as a (v8si -> v8si), d wider than c (-> v16si).  */

void __attribute__((noipa)) __GIMPLE() f7 ()
{
  v8si a_; v8si b_; v8si c_; v16si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v8si) { 0, 9, 2, 15, 0, 7, 3, 15 });
  d_ = __VEC_PERM (c_, c_, _Literal (v16si) {
      15, 8, 0, 11, 9, 4, 6, 5, 10, 14, 15, 7, 10, 12, 8, 6 });
  r16 = d_;
}

void __attribute__((noipa)) __GIMPLE() f8 ()
{
  v8si a_; v8si b_; v8si c_; v16si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v8si) { 13, 6, 6, 12, 7, 10, 6, 4 });
  d_ = __VEC_PERM (c_, c_, _Literal (v16si) {
      4, 15, 11, 1, 2, 8, 5, 3, 14, 15, 8, 6, 13, 12, 15, 10 });
  r16 = d_;
}

/* c as wide as a (v8si -> v8si), d as wide as c (-> v8si).  */

void __attribute__((noipa)) __GIMPLE() f9 ()
{
  v8si a_; v8si b_; v8si c_; v8si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v8si) { 14, 10, 2, 1, 8, 1, 8, 11 });
  d_ = __VEC_PERM (c_, c_, _Literal (v8si) { 9, 0, 4, 12, 14, 6, 0, 8 });
  r8 = d_;
}

void __attribute__((noipa)) __GIMPLE() f10 ()
{
  v8si a_; v8si b_; v8si c_; v8si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v8si) { 7, 4, 1, 3, 14, 3, 11, 2 });
  d_ = __VEC_PERM (c_, c_, _Literal (v8si) { 6, 6, 15, 8, 5, 0, 15, 1 });
  r8 = d_;
}

/* c as wide as a (v8si -> v8si), d narrower than c (-> v4si).  */

void __attribute__((noipa)) __GIMPLE() f11 ()
{
  v8si a_; v8si b_; v8si c_; v4si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v8si) { 5, 7, 8, 11, 5, 12, 7, 2 });
  d_ = __VEC_PERM (c_, c_, _Literal (v4si) { 13, 12, 4, 14 });
  r4 = d_;
}

void __attribute__((noipa)) __GIMPLE() f12 ()
{
  v8si a_; v8si b_; v8si c_; v4si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v8si) { 14, 6, 0, 12, 10, 14, 10, 6 });
  d_ = __VEC_PERM (c_, c_, _Literal (v4si) { 3, 3, 6, 7 });
  r4 = d_;
}

/* c narrower than a (v8si -> v4si), d wider than c (-> v8si).  */

void __attribute__((noipa)) __GIMPLE() f13 ()
{
  v8si a_; v8si b_; v4si c_; v8si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v4si) { 12, 2, 9, 10 });
  d_ = __VEC_PERM (c_, c_, _Literal (v8si) { 4, 0, 5, 1, 0, 7, 5, 6 });
  r8 = d_;
}

void __attribute__((noipa)) __GIMPLE() f14 ()
{
  v8si a_; v8si b_; v4si c_; v8si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v4si) { 8, 15, 0, 6 });
  d_ = __VEC_PERM (c_, c_, _Literal (v8si) { 1, 6, 0, 2, 5, 2, 7, 2 });
  r8 = d_;
}

/* c narrower than a (v8si -> v4si), d as wide as c (-> v4si).  */

void __attribute__((noipa)) __GIMPLE() f15 ()
{
  v8si a_; v8si b_; v4si c_; v4si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v4si) { 14, 15, 2, 7 });
  d_ = __VEC_PERM (c_, c_, _Literal (v4si) { 7, 4, 2, 4 });
  r4 = d_;
}

void __attribute__((noipa)) __GIMPLE() f16 ()
{
  v8si a_; v8si b_; v4si c_; v4si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v4si) { 9, 12, 6, 9 });
  d_ = __VEC_PERM (c_, c_, _Literal (v4si) { 2, 4, 7, 3 });
  r4 = d_;
}

/* c narrower than a (v8si -> v4si), d narrower than c (-> v2si).  */

void __attribute__((noipa)) __GIMPLE() f17 ()
{
  v8si a_; v8si b_; v4si c_; v2si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v4si) { 13, 3, 0, 12 });
  d_ = __VEC_PERM (c_, c_, _Literal (v2si) { 0, 0 });
  r2 = d_;
}

void __attribute__((noipa)) __GIMPLE() f18 ()
{
  v8si a_; v8si b_; v4si c_; v2si d_;
  a_ = a; b_ = b;
  c_ = __VEC_PERM (a_, b_, _Literal (v4si) { 12, 3, 15, 2 });
  d_ = __VEC_PERM (c_, c_, _Literal (v2si) { 2, 1 });
  r2 = d_;
}

/* Reference VEC_PERM_EXPR: OUT[i] = concat (X, Y)[SEL[i] % (2 * NX)].  */
static void
perm (int *out, const int *x, const int *y, int nx, const int *sel, int n)
{
  int tmp[32];
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
check (const char *name, const int *got, const int *sel0, int nc,
       const int *sel1, int nd)
{
  int c[32], want[32];
  perm (c, (int *) &a, (int *) &b, 8, sel0, nc);
  perm (want, c, c, nc, sel1, nd);
  for (int i = 0; i < nd; i++)
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
  for (int i = 0; i < 8; i++)
    {
      a[i] = i + 1;
      b[i] = i + 101;
    }

  {
    static const int s0[] = {
        14, 14, 14, 6, 5, 15, 5, 3, 14, 9, 4, 2, 1, 12, 14, 5 };
    static const int s1[] = {
        0, 4, 3, 2, 12, 15, 1, 29, 20, 28, 12, 14, 18, 31, 0, 5, 29, 17, 26,
        5, 16, 20, 14, 18, 1, 4, 6, 25, 6, 18, 24, 4 };
    f1 ();
    check ("f1", (int *) &r32, s0, 16, s1, 32);
  }
  {
    static const int s0[] = {
        0, 0, 6, 6, 1, 15, 12, 12, 13, 2, 6, 8, 10, 2, 9, 10 };
    static const int s1[] = {
        0, 26, 7, 8, 15, 6, 0, 3, 29, 31, 11, 12, 28, 12, 8, 26, 24, 7, 25,
        26, 13, 0, 17, 19, 1, 13, 11, 25, 6, 2, 9, 13 };
    f2 ();
    check ("f2", (int *) &r32, s0, 16, s1, 32);
  }
  {
    static const int s0[] = {
        14, 8, 0, 10, 9, 12, 2, 2, 2, 6, 7, 0, 11, 11, 14, 4 };
    static const int s1[] = {
        30, 8, 24, 11, 9, 19, 14, 15, 12, 10, 12, 24, 30, 5, 26, 3 };
    f3 ();
    check ("f3", (int *) &r16, s0, 16, s1, 16);
  }
  {
    static const int s0[] = {
        3, 3, 1, 8, 7, 12, 8, 13, 15, 9, 5, 2, 4, 7, 15, 2 };
    static const int s1[] = {
        17, 13, 13, 1, 4, 17, 26, 28, 15, 3, 2, 11, 18, 23, 8, 5 };
    f4 ();
    check ("f4", (int *) &r16, s0, 16, s1, 16);
  }
  {
    static const int s0[] = {
        11, 4, 14, 10, 4, 1, 0, 15, 11, 9, 1, 0, 2, 15, 2, 9 };
    static const int s1[] = { 20, 8, 4, 4, 28, 23, 2, 8 };
    f5 ();
    check ("f5", (int *) &r8, s0, 16, s1, 8);
  }
  {
    static const int s0[] = {
        10, 11, 2, 15, 2, 13, 0, 15, 0, 12, 12, 0, 2, 2, 2, 3 };
    static const int s1[] = { 16, 26, 21, 24, 29, 28, 29, 5 };
    f6 ();
    check ("f6", (int *) &r8, s0, 16, s1, 8);
  }
  {
    static const int s0[] = { 0, 9, 2, 15, 0, 7, 3, 15 };
    static const int s1[] = {
        15, 8, 0, 11, 9, 4, 6, 5, 10, 14, 15, 7, 10, 12, 8, 6 };
    f7 ();
    check ("f7", (int *) &r16, s0, 8, s1, 16);
  }
  {
    static const int s0[] = { 13, 6, 6, 12, 7, 10, 6, 4 };
    static const int s1[] = {
        4, 15, 11, 1, 2, 8, 5, 3, 14, 15, 8, 6, 13, 12, 15, 10 };
    f8 ();
    check ("f8", (int *) &r16, s0, 8, s1, 16);
  }
  {
    static const int s0[] = { 14, 10, 2, 1, 8, 1, 8, 11 };
    static const int s1[] = { 9, 0, 4, 12, 14, 6, 0, 8 };
    f9 ();
    check ("f9", (int *) &r8, s0, 8, s1, 8);
  }
  {
    static const int s0[] = { 7, 4, 1, 3, 14, 3, 11, 2 };
    static const int s1[] = { 6, 6, 15, 8, 5, 0, 15, 1 };
    f10 ();
    check ("f10", (int *) &r8, s0, 8, s1, 8);
  }
  {
    static const int s0[] = { 5, 7, 8, 11, 5, 12, 7, 2 };
    static const int s1[] = { 13, 12, 4, 14 };
    f11 ();
    check ("f11", (int *) &r4, s0, 8, s1, 4);
  }
  {
    static const int s0[] = { 14, 6, 0, 12, 10, 14, 10, 6 };
    static const int s1[] = { 3, 3, 6, 7 };
    f12 ();
    check ("f12", (int *) &r4, s0, 8, s1, 4);
  }
  {
    static const int s0[] = { 12, 2, 9, 10 };
    static const int s1[] = { 4, 0, 5, 1, 0, 7, 5, 6 };
    f13 ();
    check ("f13", (int *) &r8, s0, 4, s1, 8);
  }
  {
    static const int s0[] = { 8, 15, 0, 6 };
    static const int s1[] = { 1, 6, 0, 2, 5, 2, 7, 2 };
    f14 ();
    check ("f14", (int *) &r8, s0, 4, s1, 8);
  }
  {
    static const int s0[] = { 14, 15, 2, 7 };
    static const int s1[] = { 7, 4, 2, 4 };
    f15 ();
    check ("f15", (int *) &r4, s0, 4, s1, 4);
  }
  {
    static const int s0[] = { 9, 12, 6, 9 };
    static const int s1[] = { 2, 4, 7, 3 };
    f16 ();
    check ("f16", (int *) &r4, s0, 4, s1, 4);
  }
  {
    static const int s0[] = { 13, 3, 0, 12 };
    static const int s1[] = { 0, 0 };
    f17 ();
    check ("f17", (int *) &r2, s0, 4, s1, 2);
  }
  {
    static const int s0[] = { 12, 3, 15, 2 };
    static const int s1[] = { 2, 1 };
    f18 ();
    check ("f18", (int *) &r2, s0, 4, s1, 2);
  }

  if (fails)
    __builtin_abort ();
  return 0;
}
