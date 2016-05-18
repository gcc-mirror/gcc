/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-reassoc1" } */

unsigned f1 (unsigned x, unsigned z)
{
    unsigned y = x + z;
    y = y + x;
    y = y + x;
    y = y + x;
    y = y + x;
    y = y + x;
    y = y + x;
    return y;
}

/* { dg-final { scan-tree-dump-times "\\\* 7" 1 "reassoc1" } } */

unsigned f2 (unsigned x, unsigned z)
{
    unsigned y = x + z;
    y = y + x;
    y = y + x;
    y = y + x;
    y = y + z;
    y = y + z;
    y = y + z;
    y = y + z;
    return y;
}

/* { dg-final { scan-tree-dump-times "\\\* 5" 1 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "\\\* 4" 1 "reassoc1" } } */

unsigned f3 (unsigned x, unsigned z, unsigned k)
{
    unsigned y = x + z;
    y = y + x;
    y = y + z;
    y = y + z;
    y = y + k;
    return y;
}

/* { dg-final { scan-tree-dump-times "\\\* 2" 1 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "\\\* 3" 1 "reassoc1" } } */

unsigned f4 (unsigned x, unsigned z, unsigned k)
{
    unsigned y = k + x;
    y = y + z;
    y = y + z;
    y = y + z;
    y = y + z;
    y = y + z;
    y = y + z;
    y = y + z;
    y = y + z;
    return y;
}
/* { dg-final { scan-tree-dump-times "\\\* 8" 1 "reassoc1" } } */

unsigned f5 (unsigned x, unsigned y, unsigned z)
{
    return x + y + y + y + y + y \
      + y + z + z + z + z + z + z + z + z + z;
}

/* { dg-final { scan-tree-dump-times "\\\* 6" 1 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "\\\* 9" 1 "reassoc1" } } */

