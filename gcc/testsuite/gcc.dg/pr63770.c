/* { dg-do compile } */

char a;

struct S
{
  int f0:9;
};

volatile struct S b;

int
fn1 ()
{
  return (1 & b.f0) < a;
}
