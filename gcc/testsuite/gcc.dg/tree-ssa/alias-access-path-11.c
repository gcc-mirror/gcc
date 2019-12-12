/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-aliasing -fdump-tree-fre3" } */
typedef int outerarray[10][10][10];
typedef int innerarray[10][10];
outerarray *barptr;

int
test(int i,int j)
{
  innerarray *innerptr = (innerarray *)barptr;
  (*barptr)[i][2][j]=10;;
  (*innerptr)[3][j]=11;
  return (*barptr)[i][2][j];
}
/* { dg-final { scan-tree-dump-times "return 10" 1 "fre3" { xfail *-*-* } } } */
