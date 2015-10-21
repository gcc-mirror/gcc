/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */
struct a {int a,b;};
struct a ret(int val)
{
   return val? (struct a){} : (struct a){};
}
/* { dg-final { scan-tree-dump-not "if " "pre"} } */
