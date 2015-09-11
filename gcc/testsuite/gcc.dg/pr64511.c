/* PR debug/64511 */
/* { dg-do compile } */
/* { dg-options "-O3 -g" } */

int a, c;
int *volatile b;

void
foo (int p)
{
  int d;
  int *e = &a;
  d = ((p == 0) & *e) != 0;
  b = e;
  for (; c;)
    ;
}

void
bar (void)
{
  foo (1);
}
