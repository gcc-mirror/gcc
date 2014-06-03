/* { dg-options "-O -fselective-scheduling -fschedule-insns -fsel-sched-pipelining -fsel-sched-pipelining-outer-loops -fno-tree-dominator-opts"  } */

extern int n;
extern void bar (void);
extern int baz (int);

void
foo (void)
{
  int i, j;
  for (j = 0; j < n; j++)
    {
      for (i = 1; i < j; i++)
	bar ();
      baz (0);
    }
}
