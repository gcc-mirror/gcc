/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized"  } */
int
main()
{
  int a,b;
  __attribute__ ((noinline))
  void kill_me()
  {
    a=1234;
    b=2234;
  }
  a=0;
  b=1234;
  __attribute__ ((noinline))
  int reta()
  {
    return a;
  }
  return reta();
}
/* { dg-final { scan-tree-dump-not "kill_me" "optimized" } } */
/* { dg-final { scan-tree-dump-not "1234" "optimized" } } */
