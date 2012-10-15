/* { dg-do compile } */
/* { dg-options "isa_rev>=2" } */
/* { dg-final { scan-assembler "\td?ext\t" } } */
/* { dg-final { scan-assembler "\td?ins\t" } } */

struct A 
{
  unsigned int i : 2;
  unsigned int j : 3;
  unsigned int k : 4;
  unsigned int l : 5;
};

void func (struct A);

NOMIPS16 unsigned int f1 (struct A a)
{
  return a.j;
}

NOMIPS16 struct A f2 (struct A a, int i)
{
  a.j = i;
  return a;
}
