/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-original" } */
/* { dg-final { scan-tree-dump-times "return s \\^ x;" 4 "original" } } */

int f1 (int x, int s)
{
 return ~(~(x|s)|x)|~(~(x|s)|s);
}

int f2 (int x, int s)
{
 return ~(~(~x&s)&~(x&~s));
}

int f3 (int x, int s)
{
 return ~((x|~s)&(~x|s));
}

int f4 (int x, int s)
{
 return (x|~s)^(~x|s);
}
