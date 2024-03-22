/* PR tree-optimization/114396 */
/* { dg-additional-options "-fwrapv -fno-vect-cost-model" } */

short a = 0xF;
short b[16];
unsigned short ua = 0xF;
unsigned short ub[16];

short
__attribute__((noipa))
foo (short a)
{
  for (int e = 0; e < 9; e += 1)
    b[e] = a *= 5;
  return a;
}

short
__attribute__((noipa))
foo1 (short a)
{
  for (int e = 0; e < 9; e += 1)
    b[e] = a *= -5;
  return a;
}

unsigned short
__attribute__((noipa))
foou (unsigned short a)
{
  for (int e = 0; e < 9; e += 1)
    ub[e] = a *= -5;
  return a;
}

unsigned short
__attribute__((noipa))
foou1 (unsigned short a)
{
  for (int e = 0; e < 9; e += 1)
    ub[e] = a *= 5;
  return a;
}

short
__attribute__((noipa,optimize("O3")))
foo_o3 (short a)
{
  for (int e = 0; e < 9; e += 1)
    b[e] = a *= 5;
  return a;
}

short
__attribute__((noipa,optimize("O3")))
foo1_o3 (short a)
{
  for (int e = 0; e < 9; e += 1)
    b[e] = a *= -5;
  return a;
}

unsigned short
__attribute__((noipa,optimize("O3")))
foou_o3 (unsigned short a)
{
  for (int e = 0; e < 9; e += 1)
    ub[e] = a *= -5;
  return a;
}

unsigned short
__attribute__((noipa,optimize("O3")))
foou1_o3 (unsigned short a)
{
  for (int e = 0; e < 9; e += 1)
    ub[e] = a *= 5;
  return a;
}

int main() {
  unsigned short uexp, ures;
  short exp, res;
  exp = foo (a);
  res = foo_o3 (a);
  if (exp != res)
    __builtin_abort ();

  exp = foo1 (a);
  res = foo1_o3 (a);
  if (exp != res)
    __builtin_abort ();

  uexp = foou (a);
  ures = foou_o3 (a);
  if (uexp != ures)
    __builtin_abort ();

  uexp = foou1 (a);
  ures = foou1_o3 (a);
  if (uexp != ures)
    __builtin_abort ();

  return 0;
}
