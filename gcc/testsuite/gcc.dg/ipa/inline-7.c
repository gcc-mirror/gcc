/* Check that early inliner works out that a is empty of parameter 0.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-einline-optimized -fopt-info-inline -fno-partial-inlining -fno-inline-functions"  } */
void t(void);
int a (int b)
{
  if (!b)
   {
	t();
	t();
	t();
	t();
	t();
	t();
	t();
   }
}
void
m()
{
 a(1); /* { dg-optimized "Inlining a/\[0-9\]* into m/\[0-9\]*" } */
 a(0);
}
/* { dg-final { scan-tree-dump-times "Inlining a.* into m.*" 1 "einline"  } } */
