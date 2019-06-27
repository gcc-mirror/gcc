/* PR tree-optimization/89060 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-tailc" } */
/* { dg-final { scan-tree-dump-not "baz \\\(1\\\); \\\[tail call\\\]" "tailc" } } */
/* { dg-final { scan-tree-dump "baz \\\(2\\\); \\\[tail call\\\]" "tailc" } } */
/* { dg-final { scan-tree-dump "baz \\\(3\\\); \\\[tail call\\\]" "tailc" } } */
/* { dg-final { scan-tree-dump "baz \\\(4\\\); \\\[tail call\\\]" "tailc" } } */
/* { dg-final { scan-tree-dump-not "baz \\\(5\\\); \\\[tail call\\\]" "tailc" } } */

void qux (char *, int n);
int baz (int);

int
foo (int n)
{
  char buf[64];
  qux (buf, n);
  return baz (1);
}

int
bar (int n)
{
  {
    char buf[64];
    qux (buf, n);
  }
  return baz (2);
}

int
quux (int n)
{
  if (n < 10)
    {
      {
        char buf[64];
        qux (buf, n);
      }
      return baz (3);
    }
  if (n > 20)
    {
      {
        char buf2[64];
	qux (buf2, n + 1);
      }
      return baz (4);
    }
  char buf3[64];
  qux (buf3, n + 2);
  return baz (5);
}
