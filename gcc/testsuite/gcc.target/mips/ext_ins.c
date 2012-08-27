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

NOMIPS16 void f2 (int i)
{
  struct A c;
  c.j = i;
  func (c);
}
