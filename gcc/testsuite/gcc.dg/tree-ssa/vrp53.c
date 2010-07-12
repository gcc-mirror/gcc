/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

int
f1 (int x)
{
  x &= 0xff;
  x += 0x400;
  x &= 0x7ff;
  return x;
}

int
f2 (int x)
{
  x &= 0xff;
  x += 0x5400;
  x |= 0x4400;
  return x;
}

/* { dg-final { scan-tree-dump-not "\& (2047|0x7ff)" "vrp1" } } */
/* { dg-final { scan-tree-dump-not "\\| (17408|0x4400)" "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
