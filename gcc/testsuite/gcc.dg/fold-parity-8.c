/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo(unsigned short x)
{
  unsigned short t1 = __builtin_bswap16(x);
  unsigned int t2 = t1;
  return __builtin_parity (t2);
}

int fool(unsigned short x)
{
  unsigned short t1 = __builtin_bswap16(x);
  unsigned long t2 = t1;
  return __builtin_parityl (t2);
}

int fooll(unsigned short x)
{
  unsigned short t1 = __builtin_bswap16(x);
  unsigned long long t2 = t1;
  return __builtin_parityll (t2);
}

/* { dg-final { scan-tree-dump-not "bswap" "optimized" } } */
