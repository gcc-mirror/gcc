/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

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

/* We expect threadfull1 to eliminate the call to foo(), but not all targets
   manage that at that point.  Calling conventions (mandatory promotion) play a
   role, but there's more than that.  */
/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */
