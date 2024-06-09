/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-require-effective-target int32plus } */

int test_and4(unsigned int a)
{
  return __builtin_parity(a&4);
}

int test_and4l(unsigned long b)
{
  return __builtin_parityl(b&4);
}

int test_and4ll(unsigned long long c)
{
  return __builtin_parityll(c&4);
}

int test_shift(unsigned int d)
{
  int bits = 8*sizeof(unsigned int)-1;
  return __builtin_parity(d<<31);
}

int test_shiftl(unsigned long e)
{
  int bits = 8*sizeof(unsigned long)-1;
  return __builtin_parityl(e<<bits);
}

int test_shiftll(unsigned long long f)
{
  int bits = 8*sizeof(unsigned long long)-1;
  return __builtin_parityll(f<<bits);
}

/* { dg-final { scan-tree-dump-times "parity" 0 "optimized" } } */

