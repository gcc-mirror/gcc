/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-sccp-details" } */
/* { dg-final { scan-tree-dump-times {final value replacement} 12 "sccp" } } */

unsigned int
__attribute__((noipa))
foo (unsigned int tmp)
{
  for (int bit = 0; bit < 32; bit += 3)
    tmp &= ~(1U << bit);
  return tmp;
}

unsigned int
__attribute__((noipa))
foo1 (unsigned int tmp)
{
  for (int bit = 31; bit >= 0; bit -= 3)
    tmp &= ~(1U << bit);
  return tmp;
}

unsigned int
__attribute__((noipa))
foo2 (unsigned int tmp)
{
  for (int bit = 0; bit < 32; bit += 3)
    tmp &= (1U << bit);
  return tmp;
}

unsigned int
__attribute__((noipa))
foo3 (unsigned int tmp)
{
  for (int bit = 31; bit >= 0; bit -= 3)
    tmp &= (1U << bit);
  return tmp;
}

unsigned int
__attribute__((noipa))
foo4 (unsigned int tmp)
{
  for (int bit = 0; bit < 32; bit += 3)
    tmp |= ~(1U << bit);
  return tmp;
}

unsigned int
__attribute__((noipa))
foo5 (unsigned int tmp)
{
  for (int bit = 31; bit >= 0; bit -= 3)
    tmp |= ~(1U << bit);
  return tmp;
}

unsigned int
__attribute__((noipa))
foo6 (unsigned int tmp)
{
  for (int bit = 0; bit < 32; bit += 3)
    tmp |= (1U << bit);
  return tmp;
}

unsigned int
__attribute__((noipa))
foo7 (unsigned int tmp)
{
  for (int bit = 31; bit >= 0; bit -= 3)
    tmp |= (1U << bit);
  return tmp;
}

unsigned int
__attribute__((noipa))
foo8 (unsigned int tmp)
{
  for (int bit = 0; bit < 32; bit += 3)
    tmp ^= ~(1U << bit);
  return tmp;
}

unsigned int
__attribute__((noipa))
foo9 (unsigned int tmp)
{
  for (int bit = 31; bit >= 0; bit -= 3)
    tmp ^= ~(1U << bit);
  return tmp;
}

unsigned int
__attribute__((noipa))
foo10 (unsigned int tmp)
{
  for (int bit = 0; bit < 32; bit += 3)
    tmp ^= (1U << bit);
  return tmp;
}

unsigned int
__attribute__((noipa))
foo11 (unsigned int tmp)
{
  for (int bit = 31; bit >= 0; bit -= 3)
    tmp ^= (1U << bit);
  return tmp;
}
