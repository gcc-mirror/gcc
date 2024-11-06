/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-phiopt1-details -fdump-tree-optimized" } */

int h(void);
int h1(void);

int f(int a, int b, int d)
{
  int c;
  if (a < 0)
  {
        a = h();
        c = d > 0 ? d : -d;
        a += h();
  }
  else
  {
        a = h1();
        c = d > 0 ? d : -d;
        a += h1();
  }
  return a + c;
}

/* ABS <d> should be not to pulled out of the if statement early on in phiopt1 as it lengthes d's
   live range over a call.
   Note pre can lift the  */
/* { dg-final { scan-tree-dump-not "changed to factor operation out from " "phiopt1" } } */
/* { dg-final { scan-tree-dump "if " "phiopt1" } } */
/* { dg-final { scan-tree-dump "if " "optimized" } } */
/* There will be 2 ABS due to the need for it in the inner if. */
/* { dg-final { scan-tree-dump-times "ABS_EXPR " 2 "phiopt1" } } */
/* { dg-final { scan-tree-dump-times "ABS_EXPR " 2 "optimized" } } */
