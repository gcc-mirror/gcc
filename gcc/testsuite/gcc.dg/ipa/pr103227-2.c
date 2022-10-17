/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-slim"  } */

struct S
{
  int a, b, c;
};

int ellide (int c);

static void __attribute__ ((noinline))
foo (struct S s)
{
  int c = s.c;
  if (c != 21)
    ellide (c);
}

void
entry (void)
{
  struct S s;
  s.a = 1;
  s.b = 64;
  s.c = 21;
  foo (s);
}

/* { dg-final { scan-tree-dump-not "ellide" "optimized" } } */
