// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96429
// { dg-do compile }
// { dg-options "-fdump-tree-original" }
ptrdiff_t subbyte(byte* bp1, byte* bp2)
{
    // { dg-final { scan-tree-dump "\\\(bp1 - bp2\\\) /\\\[ex\\\] 1;" "original" } }
    return bp1 - bp2;
}

ptrdiff_t subshort(short* sp1, short* sp2)
{
    // { dg-final { scan-tree-dump "\\\(sp1 - sp2\\\) /\\\[ex\\\] 2;" "original" } }
    return sp1 - sp2;
}

ptrdiff_t subint(int* ip1, int* ip2)
{
    // { dg-final { scan-tree-dump "\\\(ip1 - ip2\\\) /\\\[ex\\\] 4;" "original" } }
    return ip1 - ip2;
}

ptrdiff_t sublong(long* lp1, long* lp2)
{
    // { dg-final { scan-tree-dump "\\\(lp1 - lp2\\\) /\\\[ex\\\] 8;" "original" } }
    return lp1 - lp2;
}
