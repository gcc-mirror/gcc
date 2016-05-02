/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce2 -fdump-tree-forwprop1-details" } */
  
int abarney[2];
int afred[1];
 
void foo(int edx, int eax)
{
  if (eax == 100)
    {
      if (edx == 1)
        {
          abarney[0] = 5;
          abarney[1] = 6;
        }
    }
  if (eax == 100)
    {
      if (-- edx == 0)
        afred[0] = 2;
    }
}
 

/* Verify that we did a forward propagation.  */
/* { dg-final { scan-tree-dump-times "gimple_simplified" 1 "forwprop1"} } */

/* After cddce we should have two IF statements remaining as the other
   two tests can be threaded.  */
/* { dg-final { scan-tree-dump-times "if " 2 "cddce2"} } */
