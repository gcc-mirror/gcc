/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-einline" } */

void bar (char *);
int i;

static void
foo (void)
{
  char *p = __builtin_alloca (i);
  bar (p);
}

int
baz (void)
{
  foo ();	/* foo() should not be inlined here because it calls alloca */
  return 6;
}

/* { dg-final { scan-tree-dump-times "Inlining foo into baz" 0 "einline"} } */
