/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-original" } */

int f1 (int a, int b)
{
 return ~(a|b)|(~a&b);
}

int f2 (int a, int b)
{
 return (a|b)^(a|~b);
}

/* { dg-final { scan-tree-dump-times "return \\~a;" 2 "original" } } */

int f3 (int a, int b)
{
 return ~(a|b)|(a&b);
}

/* { dg-final { scan-tree-dump "return \\~\\(a \\^ b\\);" "original" } } */

int f4 (int a, int b)
{
 return a^b^(~a|b);
}

/* { dg-final { scan-tree-dump "return \\~b \\| a;" "original" } } */

int f5 (int a, int b)
{
 return (a^b)|~(a|b);
}

/* { dg-final { scan-tree-dump "return \\~\\(a \\& b\\);" "original" } } */
