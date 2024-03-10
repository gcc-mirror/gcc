/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-release_ssa" } */
int a;
int ret1()
{
	return a;
}
int inline
__attribute__((always_inline)) aret1()
{
	return ret1();
}
int test()
{
  return aret1();
}
/* { dg-final { scan-tree-dump-not "= ret1" "release_ssa" } }  */
