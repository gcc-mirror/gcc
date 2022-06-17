/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-sccp-details" } */
/* { dg-final { scan-tree-dump-times {final value replacement} 12 "sccp" } } */

unsigned short
__attribute__((noipa))
foo (unsigned short tmp)
{
  for (int bit = 0; bit < 16; bit += 3)
    tmp &= ~(1U << bit);
  return tmp;
}

unsigned short
__attribute__((noipa))
foo1 (unsigned short tmp)
{
  for (int bit = 15; bit >= 0; bit -= 3)
    tmp &= ~(1U << bit);
  return tmp;
}

unsigned short
__attribute__((noipa))
foo2 (unsigned short tmp)
{
  for (int bit = 0; bit < 16; bit += 3)
    tmp &= (1U << bit);
  return tmp;
}

unsigned short
__attribute__((noipa))
foo3 (unsigned short tmp)
{
  for (int bit = 15; bit >= 0; bit -= 3)
    tmp &= (1U << bit);
  return tmp;
}

unsigned short
__attribute__((noipa))
foo4 (unsigned short tmp)
{
  for (int bit = 0; bit < 16; bit += 3)
    tmp |= ~(1U << bit);
  return tmp;
}

unsigned short
__attribute__((noipa))
foo5 (unsigned short tmp)
{
  for (int bit = 15; bit >= 0; bit -= 3)
    tmp |= ~(1U << bit);
  return tmp;
}

unsigned short
__attribute__((noipa))
foo6 (unsigned short tmp)
{
  for (int bit = 0; bit < 16; bit += 3)
    tmp |= (1U << bit);
  return tmp;
}

unsigned short
__attribute__((noipa))
foo7 (unsigned short tmp)
{
  for (int bit = 15; bit >= 0; bit -= 3)
    tmp |= (1U << bit);
  return tmp;
}

unsigned short
__attribute__((noipa))
foo8 (unsigned short tmp)
{
  for (int bit = 0; bit < 16; bit += 3)
    tmp ^= ~(1U << bit);
  return tmp;
}

unsigned short
__attribute__((noipa))
foo9 (unsigned short tmp)
{
  for (int bit = 15; bit >= 0; bit -= 3)
    tmp ^= ~(1U << bit);
  return tmp;
}

unsigned short
__attribute__((noipa))
foo10 (unsigned short tmp)
{
  for (int bit = 0; bit < 16; bit += 3)
    tmp ^= (1U << bit);
  return tmp;
}

unsigned short
__attribute__((noipa))
foo11 (unsigned short tmp)
{
  for (int bit = 15; bit >= 0; bit -= 3)
    tmp ^= (1U << bit);
  return tmp;
}
