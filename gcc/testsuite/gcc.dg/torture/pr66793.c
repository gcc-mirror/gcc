/* { dg-do link } */

int a, b, c; 

struct S0
{
  int f1;
} *d; 

void
fn1 (struct S0 p)
{
  for (p.f1 = 0; p.f1 < 1; p.f1++)
    c = a && b ? a && b : 1; 
  for (; c;)
    ;
}

int
main ()
{
  struct S0 **f = &d;
  d = 0; 
  fn1 (**f); 
  return 0;
}
