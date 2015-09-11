
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt1-details" } */

extern unsigned short mode_size[];

int
oof (int mode)
{
  return (64 < mode_size[mode] ? 64 : mode_size[mode]);
}

/* { dg-final { scan-tree-dump-times "factor conversion out" 1 "phiopt1" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 1 "phiopt1" } } */

