/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

int f (int a, int b)
{
    if (a != b) __builtin_unreachable ();
    return a / b;
}
/* { dg-final { scan-tree-dump-times "return 1;" 1 "evrp" } } */

int f2 (int a, int b)
{
    if (a != b) __builtin_unreachable ();
    return a % b;
}
/* { dg-final { scan-tree-dump-times "return 0;" 1 "evrp" } } */
