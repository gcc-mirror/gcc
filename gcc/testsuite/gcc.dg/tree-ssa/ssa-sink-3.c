/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-sink-stats" } */
extern void foo(int a);
int
main (int argc)
{
  int a;
  a = argc + 1;
  if (argc + 3)
    {
      foo (a);
    }
}
/* We should sink the a = argc + 1 calculation into the if branch  */
/* { dg-final { scan-tree-dump-times "Sunk statements: 1" 1 "sink" } } */
/* { dg-final { cleanup-tree-dump "sink" } } */
