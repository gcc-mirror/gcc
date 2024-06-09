/* PR middle-end/113596 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-einline" } */
/* { dg-final { scan-tree-dump-times "__builtin_stack_save \\\(" 3 "einline" } } */
/* { dg-final { scan-tree-dump-times "__builtin_stack_restore \\\(" 3 "einline" } } */

void baz (char *p, int n);
volatile int v;

static inline __attribute__((always_inline)) void
foo (int n)
{
  ++v;
  {
    char *p = __builtin_alloca (n);
    baz (p, n);
  }
  ++v;
}

static inline __attribute__((always_inline)) void
bar (int n)
{
  ++v;
  {
    char p[n];
    baz (p, n);
  }
  ++v;
}

void
qux (int n)
{
  foo (n);
  bar (n);
}
