/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce1" } */

int test_shiftmax(unsigned int a)
{
  return __builtin_popcount(a>>(8*sizeof(a)-1));
}

int test_shiftmaxl(unsigned long b)
{
  return __builtin_popcountl(b>>(8*sizeof(b)-1));
}

int test_shiftmaxll(unsigned long long c)
{
  return __builtin_popcountll(c>>(8*sizeof(c)-1));
}

int test_shift7(unsigned char d)
{
  return __builtin_popcount(d>>7);
}

int test_shift7l(unsigned char e)
{
  return __builtin_popcountl(e>>7);
}

int test_shift7ll(unsigned char f)
{
  return __builtin_popcountll(f>>7);
}

int test_shift15(unsigned short g)
{
  return __builtin_popcount(g>>15);
}

int test_shift15l(unsigned short h)
{
  return __builtin_popcountl(h>>15);
}

int test_shift15ll(unsigned short i)
{
  return __builtin_popcountll(i>>15);
}

/* { dg-final { scan-tree-dump-times "popcount" 0 "cddce1" } } */

