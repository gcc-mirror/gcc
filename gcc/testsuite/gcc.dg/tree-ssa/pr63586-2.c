/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fdump-tree-reassoc1" } */

float f1_float (float x, float z)
{
    float y = x + z;
    y = y + x;
    y = y + x;
    y = y + x;
    y = y + x;
    y = y + x;
    y = y + x;
    y = y + x;
    return y;
}

float f1_float2 (float x)
{
    float y = x + 3 * x + x;
    return y;
}

int f1_int (int x)
{
    int y = x + 4 * x + x;
    return y;
}

/* { dg-final { scan-tree-dump-times "\\\* 8\\\.0e\\\+0" 1 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "\\\* 5\\\.0e\\\+0" 1 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "\\\* 6" 1 "reassoc1" } } */

