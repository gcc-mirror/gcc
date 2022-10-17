/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-sccp-details" } */
/* { dg-final { scan-tree-dump-times {final value replacement} 12 "sccp" } } */

unsigned long long
__attribute__((noipa))
foo (unsigned long long tmp)
{
  for (int bit = 0; bit < 64; bit += 3)
    tmp &= ~(1ULL << bit);
  return tmp;
}

unsigned long long
__attribute__((noipa))
foo1 (unsigned long long tmp)
{
  for (int bit = 63; bit >= 0; bit -= 3)
    tmp &= ~(1ULL << bit);
  return tmp;
}

unsigned long long
__attribute__((noipa))
foo2 (unsigned long long tmp)
{
  for (int bit = 0; bit < 64; bit += 3)
    tmp &= (1ULL << bit);
  return tmp;
}

unsigned long long
__attribute__((noipa))
foo3 (unsigned long long tmp)
{
  for (int bit = 63; bit >= 0; bit -= 3)
    tmp &= (1ULL << bit);
  return tmp;
}

unsigned long long
__attribute__((noipa))
foo4 (unsigned long long tmp)
{
  for (int bit = 0; bit < 64; bit += 3)
    tmp |= ~(1ULL << bit);
  return tmp;
}

unsigned long long
__attribute__((noipa))
foo5 (unsigned long long tmp)
{
  for (int bit = 63; bit >= 0; bit -= 3)
    tmp |= ~(1ULL << bit);
  return tmp;
}

unsigned long long
__attribute__((noipa))
foo6 (unsigned long long tmp)
{
  for (int bit = 0; bit < 64; bit += 3)
    tmp |= (1ULL << bit);
  return tmp;
}

unsigned long long
__attribute__((noipa))
foo7 (unsigned long long tmp)
{
  for (int bit = 63; bit >= 0; bit -= 3)
    tmp |= (1ULL << bit);
  return tmp;
}

unsigned long long
__attribute__((noipa))
foo8 (unsigned long long tmp)
{
  for (int bit = 0; bit < 64; bit += 3)
    tmp ^= ~(1ULL << bit);
  return tmp;
}

unsigned long long
__attribute__((noipa))
foo9 (unsigned long long tmp)
{
  for (int bit = 63; bit >= 0; bit -= 3)
    tmp ^= ~(1ULL << bit);
  return tmp;
}

unsigned long long
__attribute__((noipa))
foo10 (unsigned long long tmp)
{
  for (int bit = 0; bit < 64; bit += 3)
    tmp ^= (1ULL << bit);
  return tmp;
}

unsigned long long
__attribute__((noipa))
foo11 (unsigned long long tmp)
{
  for (int bit = 63; bit >= 0; bit -= 3)
    tmp ^= (1ULL << bit);
  return tmp;
}
