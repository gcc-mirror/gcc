/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-threadfull1" } */

static char b;
static unsigned c;
void foo();
short(a)(short d, short e) { return d * e; }
static short f(short d) {
  b = 0;
  if ((d && 0 >= c < d) ^ d)
    ;
  else
    foo();
  return d;
}
int main()
{
  short g = a(5, b ^ 9854);
  f(g);
}

/* { dg-final { scan-tree-dump-not "foo" "threadfull1" } } */
