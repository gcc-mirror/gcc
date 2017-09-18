/* { dg-do compile } */

struct __attribute__((aligned (32)))
{
  char misaligner;
  int foo[100];
  int bar[100];
} *a;

void
fn1 (int n)
{
  int *b = a->foo;
  for (int i = 0; i < n; i++)
    a->bar[i] = b[i];
}

/* { dg-final { scan-tree-dump-not "Unknown misalignment" "vect" } } */
