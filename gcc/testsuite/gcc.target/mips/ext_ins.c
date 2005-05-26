/* { dg-do compile } */
/* { dg-mips-options "-march=mips32r2" } */
/* { dg-final { scan-assembler "ext" } } */
/* { dg-final { scan-assembler "ins" } } */

struct A 
{
  unsigned int i : 2;
  unsigned int j : 3;
  unsigned int k : 4;
  unsigned int l : 5;
};

void func (struct A);

unsigned int f1 (struct A a)
{
  return a.j;
}

void f2 (int i)
{
  struct A c;
  c.j = i;
  func (c);
}
