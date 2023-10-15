/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-phiopt1" } */

unsigned abs_with_convert0 (int x)
{
    unsigned int y = x;

    if (x < 0)
        y = -y;

  return y;
}
unsigned abs_with_convert1 (unsigned x)
{
    int y = x;

    if (y < 0)
        x = -x;

  return x;
}

/* { dg-final { scan-tree-dump-times "ABSU_EXPR <"  2  "phiopt1" } } */
/* { dg-final { scan-tree-dump-not   "if "        "phiopt1" } } */
