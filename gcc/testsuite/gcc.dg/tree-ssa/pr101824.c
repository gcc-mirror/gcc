/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-pcom-details -fdump-tree-optimized" } */

int main()
{
  volatile int y;
  void bar()
    {
      __builtin_printf ("%d", y);
    }
  while (y)
    ;
  return 0;
}

/* Make sure the load from y is correctly interpreted as volatile, even
   when going through FRAME.  */
/* { dg-final { scan-tree-dump-not "Executing predictive commoning" "pcom" } } */
/* { dg-final { scan-tree-dump " ={v} FRAME" "optimized" } } */
