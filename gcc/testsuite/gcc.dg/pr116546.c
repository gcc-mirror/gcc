/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

extern long foo (void);
extern long bar (void);

long
test1 (long n)
{
  n &= 7;
  if (n == 4) {
    if (n & 4)
      return foo ();
    else
      return bar ();
  }
  return 0;
}

long
test2 (long n)
{
  n &= 7;
  if (n > 4) {
    if (n & 4)
      return foo ();
    else
      return bar ();
  }
  return 0;
}

long
test3 (long n)
{
  n &= 7;
  if (n >= 4) {
    if (n & 4)
      return foo ();
    else
      return bar ();
  }
  return 0;
}

/* { dg-final { scan-tree-dump-not "bar" "evrp" } } */
