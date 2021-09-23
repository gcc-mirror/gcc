/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-evrp -fdump-tree-vrp1 -fno-tree-ccp" } */

int foo (void)
{
    volatile int a = -1;
    long long b = (1LL << (sizeof (b) * 8 - 1)); // LLONG_MIN
    long long x = (a & b); // x == 0x8000000000000000
    if (x < 1LL) { ; } else { __builtin_abort(); }
    return 0;
}

/* { dg-final { scan-tree-dump "\\\[-INF, 0\\\]" "vrp1" } } */
