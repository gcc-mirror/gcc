/* { dg-do compile } */
/* { dg-options "-O2 -fno-early-inlining -fdump-tree-fixup_cfg3" } */

int n;

static int
bar (void)
{
  int a;

  n = 0;
  a = 0;

  return n;
}

__attribute__ ((pure, returns_twice)) int
foo (void)
{
  n = bar () + 1;
  foo ();

  return 0;
}

/* Abnormal edges should be properly elided after IPA inlining of bar.  */
/* { dg-final { scan-tree-dump-times "bb" 1 "fixup_cfg3" } } */
