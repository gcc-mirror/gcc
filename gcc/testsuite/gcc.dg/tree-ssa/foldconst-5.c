/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */


static const char a[5]="t";
static const int b[5]={1,2};
static const struct a {int a : 6; int b : 6;} c = {5,9};
int
test()
{
  return a[2]+b[1]+b[3]+c.b;
}
/* { dg-final { scan-tree-dump "return 11;" "ccp1" } } */

