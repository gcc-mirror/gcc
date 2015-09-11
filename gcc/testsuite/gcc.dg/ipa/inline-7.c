/* Check that early inliner works out that a is empty of parameter 0.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-einline -fno-partial-inlining"  } */
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
 a(1);
 a(0);
}
/* { dg-final { scan-tree-dump-times "Inlining a into m" 1 "einline"  } } */
