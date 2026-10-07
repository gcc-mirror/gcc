/* Constant VEC_PERM_EXPRs with inputs wider than the result.  */
/* { dg-do run } */
/* { dg-additional-options "-fgimple -Wno-psabi" } */

typedef int v8si __attribute__((vector_size(32)));
typedef int v4si __attribute__((vector_size(16)));
typedef int v16si __attribute__((vector_size(64)));
typedef unsigned char v16qi __attribute__((vector_size(16)));
typedef unsigned char v8qi __attribute__((vector_size(8)));
typedef double v4df __attribute__((vector_size(32)));
typedef double v2df __attribute__((vector_size(16)));
typedef long long v2di __attribute__((vector_size(16)));

v8si a, b;
v4si r;
v16si c;
v8si s;
v16qi qa, qb;
v8qi qr;
v4df da, db;
v2df dr;

void __attribute__((noipa)) __GIMPLE() f1 ()
{
  v8si a_; v8si b_; v4si r_;
  a_ = a; b_ = b;
  r_ = __VEC_PERM (a_, b_, _Literal (v4si) { 0, 8, 1, 9 });
  r = r_;
}

void __attribute__((noipa)) __GIMPLE() f2 ()
{
  v8si a_; v8si b_; v4si r_;
  a_ = a; b_ = b;
  r_ = __VEC_PERM (a_, b_, _Literal (v4si) { 7, 15, 6, 14 });
  r = r_;
}

void __attribute__((noipa)) __GIMPLE() f3 ()
{
  v16qi a_; v16qi b_; v8qi r_;
  a_ = qa; b_ = qb;
  r_ = __VEC_PERM (a_, b_, _Literal (v8qi) { 31, 0, 17, 30, 15, 16, 2, 24 });
  qr = r_;
}

void __attribute__((noipa)) __GIMPLE() f4 ()
{
  v4df a_; v4df b_; v2df r_;
  a_ = da; b_ = db;
  r_ = __VEC_PERM (a_, b_, _Literal (v2di) { 6, 3 });
  dr = r_;
}

void __attribute__((noipa)) __GIMPLE() f5 ()
{
  v16si c_; v8si s_;
  c_ = c;
  s_ = __VEC_PERM (c_, _Literal (v16si) { 0 },
		   _Literal (v8si) { 2, 3, 4, 5, 6, 7, 8, 9 });
  s = s_;
}

void __attribute__((noipa)) __GIMPLE() f6 ()
{
  v16si c_; v8si s_;
  c_ = c;
  s_ = __VEC_PERM (c_, _Literal (v16si) { 0 },
		   _Literal (v8si) { 15, 16, 14, 17, 9, 31, 0, 20 });
  s = s_;
}

void __attribute__((noipa)) __GIMPLE() f7 ()
{
  v8si b_; v4si r_;
  b_ = b;
  r_ = __VEC_PERM (b_, _Literal (v8si) { 0 }, _Literal (v4si) { 2, 3, 4, 5 });
  r = r_;
}

void __attribute__((noipa)) __GIMPLE() f8 ()
{
  v8si b_; v4si r_;
  b_ = b;
  r_ = __VEC_PERM (_Literal (v8si) { 0 }, b_, _Literal (v4si) { 0, 4, 5, 6 });
  r = r_;
}

void __attribute__((noipa)) __GIMPLE() f9 ()
{
  v8si b_; v4si r_;
  b_ = b;
  r_ = __VEC_PERM (_Literal (v8si) { 0 }, b_, _Literal (v4si) { 0, 8, 9, 10 });
  r = r_;
}

int
main ()
{
  for (int i = 0; i < 8; i++)
    {
      a[i] = i;
      b[i] = 100 + i;
    }
  for (int i = 0; i < 16; i++)
    {
      c[i] = i + 1;
      qa[i] = i;
      qb[i] = 100 + i;
    }
  for (int i = 0; i < 4; i++)
    {
      da[i] = i;
      db[i] = 100 + i;
    }

  f1 ();
  if (r[0] != 0 || r[1] != 100 || r[2] != 1 || r[3] != 101)
    __builtin_abort ();

  f2 ();
  if (r[0] != 7 || r[1] != 107 || r[2] != 6 || r[3] != 106)
    __builtin_abort ();

  f3 ();
  {
    static const unsigned char e[8] = { 115, 0, 101, 114, 15, 100, 2, 108 };
    for (int i = 0; i < 8; i++)
      if (qr[i] != e[i])
	__builtin_abort ();
  }

  f4 ();
  if (dr[0] != 102 || dr[1] != 3)
    __builtin_abort ();

  f5 ();
  for (int i = 0; i < 8; i++)
    if (s[i] != i + 3)
      __builtin_abort ();

  f6 ();
  {
    static const int e[8] = { 16, 0, 15, 0, 10, 0, 1, 0 };
    for (int i = 0; i < 8; i++)
      if (s[i] != e[i])
	__builtin_abort ();
  }

  f7 ();
  if (r[0] != 102 || r[1] != 103 || r[2] != 104 || r[3] != 105)
    __builtin_abort ();

  f8 ();
  if (r[0] != 0 || r[1] != 0 || r[2] != 0 || r[3] != 0)
    __builtin_abort ();

  f9 ();
  if (r[0] != 0 || r[1] != 100 || r[2] != 101 || r[3] != 102)
    __builtin_abort ();

  return 0;
}
