/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */
/* { dg-require-effective-target int32plus } */

unsigned mul(unsigned char c) {
    if (c > 3) __builtin_unreachable();
    return c << 18 | c << 15 |
        c << 12 | c << 9 |
        c << 6 | c << 3 | c;
}
/* { dg-final { scan-tree-dump-times " \\* 299593" 1 "evrp" } } */
