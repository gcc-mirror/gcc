/* PR debug/43237 */
/* { dg-do compile } */
/* { dg-options "-gdwarf -O2 -dA -fno-merge-debug-strings" } */

struct S
{
  int *a;
  int b;
  int **c;
  int d;
};

void foo (struct S *);
void bar (struct S *);

int
baz (void)
{
  struct S s;
  foo (&s);
  {
    int a[s.b];
    int *c[s.d];
    s.a = a;
    s.c = c;
    bar (&s);
  }
  return 0;
}

/* { dg-final { scan-assembler-not "LLST\[^\\r\\n\]*DW_AT_upper_bound" } } */
