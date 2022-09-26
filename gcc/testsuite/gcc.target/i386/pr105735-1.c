/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-sccp-details" } */
/* { dg-final { scan-tree-dump-times {final value replacement} 8 "sccp" } } */

unsigned int
__attribute__((noipa))
foo (unsigned int tmp, unsigned int bit2)
{
  for (int bit = 0; bit < 64; bit++)
    tmp &= bit2;
  return tmp;
}

unsigned int
__attribute__((noipa))
foo1 (unsigned int tmp, unsigned int bit2)
{
  for (int bit = 63; bit >= 0; bit -=3)
    tmp &= bit2;
  return tmp;
}

unsigned int
__attribute__((noipa))
foo2 (unsigned int tmp, unsigned int bit2)
{
  for (int bit = 0; bit < 64; bit++)
    tmp |= bit2;
  return tmp;
}

unsigned int
__attribute__((noipa))
foo3 (unsigned int tmp, unsigned int bit2)
{
  for (int bit = 63; bit >= 0; bit -=3)
    tmp |= bit2;
  return tmp;
}

unsigned int
__attribute__((noipa))
foo4 (unsigned int tmp, unsigned int bit2)
{
  for (int bit = 0; bit < 64; bit++)
    tmp ^= bit2;
  return tmp;
}

unsigned int
__attribute__((noipa))
foo5 (unsigned int tmp, unsigned int bit2)
{
  for (int bit = 0; bit < 63; bit++)
    tmp ^= bit2;
  return tmp;
}

unsigned int
__attribute__((noipa))
f (unsigned int tmp, int bit, unsigned int bit2)
{
  unsigned int res = tmp;
  for (int i = 0; i < bit; i++)
    res &= bit2;
  return res;
}

unsigned int
__attribute__((noipa))
f1 (unsigned int tmp, int bit, unsigned int bit2)
{
  unsigned int res = tmp;
  for (int i = 0; i < bit; i++)
    res |= bit2;
  return res;
}

unsigned int
__attribute__((noipa))
f2 (unsigned int tmp, int bit, unsigned int bit2)
{
  unsigned int res = tmp;
  for (int i = 0; i < bit; i++)
    res ^= bit2;
  return res;
}

