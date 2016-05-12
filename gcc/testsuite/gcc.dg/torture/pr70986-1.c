/* { dg-do compile } */

int a, g;
char b, c;
short d, e, f;

char
fn1 ()
{
  return a ? a : 1;
}

void
fn2 ()
{
  char h;
  for (; d;)
    for (; e; e++)
      c = (fn1 () && h) & !(f |= 9 ^ (b > (g = c)));
  for (;;)
    ;
}
