/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized"  } */
void foo ();
int
test()
{
  struct {int a,b;} a = {0,0};
  __attribute__ ((noinline))
  void nested ()
  {
	  a.b++;
  }
  nested ();
  return a.a;
}
/* { dg-final { scan-tree-dump "return 0" "optimized"} } */
