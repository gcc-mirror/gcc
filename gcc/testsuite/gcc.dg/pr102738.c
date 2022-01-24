/* PR tree-optimization/102738 */
/* { dg-options "-O2 -fdump-tree-evrp" } */
/* { dg-do compile { target int128 } } */

/* Remove arithmetic shift right when the LHS is known to be 0 or -1.  */

int a1(__int128 f, int g)
{
     /* Leaves f >> 127.  */
    return (f >> 127) >> g;
}

int a2(int f, int g)
{
     /* Leaves f >> 31.  */
    return (f >> 31) >> g;
}

int a3(int f, int g)
{
    if (f == 0 || f == -1)
      return f >> g;
    __builtin_unreachable();
}

int a4(int f, int g)
{
    if (f == 0 || f == 1)
      return (-f) >> g;
    __builtin_unreachable();
}

int a5(int f, int g)
{
    if (f == 0 || f == 1)
      return (f-1) >> g;
    return 0;
}

int a6(int f, int g)
{
    if (f == 6 || f == 7)
      return (f-7) >> g;
    __builtin_unreachable();
}

/* { dg-final { scan-tree-dump-times " >> 127" 1 "evrp" } } */
/* { dg-final { scan-tree-dump-times " >> 31" 1 "evrp" } } */
/* { dg-final { scan-tree-dump-times " >> " 2 "evrp" } } */
