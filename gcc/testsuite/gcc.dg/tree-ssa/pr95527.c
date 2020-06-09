/* PR tree-optimization/95527 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return 0;" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 18;" 4 "optimized" } } */

/* { dg-final { scan-tree-dump-times "a_\[0-9]*\\\(D\\\) == 0" 1 "optimized" } } */

int
f1 (int a)
{
  return __builtin_ffs (a) == 0;
}

/* { dg-final { scan-tree-dump-times "b_\[0-9]*\\\(D\\\) != 0" 1 "optimized" } } */

int
f2 (int b)
{
  return __builtin_ffs (b) != 0;
}

int
f3 (int x)
{
  return __builtin_ffs (x) == -1;
}

int
f4 (int x)
{
  return 17 + (__builtin_ffs (x) != -1);
}

int
f5 (int x)
{
  return __builtin_ffs (x) == __SIZEOF_INT__ * __CHAR_BIT__ + 1;
}

int
f6 (int x)
{
  return 17 + (__builtin_ffs (x) != __SIZEOF_INT__ * __CHAR_BIT__ + 1);
}

/* { dg-final { scan-tree-dump-times "c_\[0-9]*\\\(D\\\) & 63" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "== 32" 1 "optimized" } } */

int
f7 (int c)
{
  return __builtin_ffs (c) == 6;
}

/* { dg-final { scan-tree-dump-times "d_\[0-9]*\\\(D\\\) & 16383" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "!= 8192" 1 "optimized" } } */

int
f8 (int d)
{
  return __builtin_ffs (d) != 14;
}

/* { dg-final { scan-tree-dump-times "e_\[0-9]*\\\(D\\\) == -9223372036854775808" 1 "optimized" { target lp64 } } } */
/* { dg-final { scan-tree-dump-times "e_\[0-9]*\\\(D\\\) == -2147483648" 1 "optimized" { target ilp32 } } } */

int
f9 (long int e)
{
  return __builtin_ffsl (e) == __SIZEOF_LONG__ * __CHAR_BIT__;
}

/* { dg-final { scan-tree-dump-times "f_\[0-9]*\\\(D\\\) != -9223372036854775808" 1 "optimized" } } */

int
f10 (long long int f)
{
  return __builtin_ffsll (f) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__;
}

/* { dg-final { scan-tree-dump-times "g_\[0-9]*\\\(D\\\) != 0" 1 "optimized" } } */

int
f11 (long long int g)
{
  return __builtin_ffsll (g) > 0;
}

/* { dg-final { scan-tree-dump-times "h_\[0-9]*\\\(D\\\) == 0" 1 "optimized" } } */

int
f12 (int h)
{
  return __builtin_ffs (h) <= 0;
}

int
f13 (int x)
{
  return 17 + (__builtin_ffs (x) > -1);
}

int
f14 (int x)
{
  return __builtin_ffs (x) <= -1;
}

int
f15 (int x)
{
  return __builtin_ffs (x) > __SIZEOF_INT__ * __CHAR_BIT__;
}

int
f16 (int x)
{
  return 17 + (__builtin_ffs (x) <= __SIZEOF_INT__ * __CHAR_BIT__);
}

/* { dg-final { scan-tree-dump-times "i_\[0-9]*\\\(D\\\) & 63" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "i_\[0-9]*\\\(D\\\) != 0" 1 "optimized" } } */

int
f17 (int i)
{
  return __builtin_ffs (i) > 6;
}

/* { dg-final { scan-tree-dump-times "j_\[0-9]*\\\(D\\\) & 4095" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "j_\[0-9]*\\\(D\\\) == 0" 1 "optimized" } } */

int
f18 (int j)
{
  return __builtin_ffs (j) <= 12;
}

/* { dg-final { scan-tree-dump-times "k_\[0-9]*\\\(D\\\) == -2147483648" 1 "optimized" { target int32 } } } */

int
f19 (int k)
{
  return __builtin_ffs (k) > __SIZEOF_INT__ * __CHAR_BIT__ - 1;
}

/* { dg-final { scan-tree-dump-times "l_\[0-9]*\\\(D\\\) != -2147483648" 1 "optimized" { target int32 } } } */

int
f20 (int l)
{
  return __builtin_ffs (l) <= __SIZEOF_INT__ * __CHAR_BIT__ - 1;
}

/* { dg-final { scan-tree-dump-times "m_\[0-9]*\\\(D\\\) & 1073741823" 1 "optimized" { target int32 } } } */
/* { dg-final { scan-tree-dump-times "m_\[0-9]*\\\(D\\\) != 0" 1 "optimized" } } */

int
f21 (int m)
{
  return __builtin_ffs (m) > __SIZEOF_INT__ * __CHAR_BIT__ - 2;
}

/* { dg-final { scan-tree-dump-times "n_\[0-9]*\\\(D\\\) & 1073741823" 1 "optimized" { target int32 } } } */
/* { dg-final { scan-tree-dump-times "n_\[0-9]*\\\(D\\\) == 0" 1 "optimized" } } */

int
f22 (int n)
{
  return __builtin_ffs (n) <= __SIZEOF_INT__ * __CHAR_BIT__ - 2;
}
