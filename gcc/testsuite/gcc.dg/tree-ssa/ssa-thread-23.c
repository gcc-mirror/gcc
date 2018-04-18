/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ethread-stats" } */

/* Test that we can thread a switch for which we have a known range.  */

extern void there();
int stuff10, stuff20;
void foo(int x)
{
  if (x == 50 || x == 80) goto doit;
  there();
doit:
  switch (x) {
  case 10:
    stuff10 = 1;
    break;
  case 20:
    stuff20 = 2;
    break;
  default:
    break;
  }
}

/* { dg-final { scan-tree-dump-times "Jumps threaded: 1" 1 "ethread" } } */
