/* { dg-do compile } */
/* { dg-require-effective-target popcount } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

const unsigned m1  = 0x55555555UL;
const unsigned m2  = 0x33333333UL;
const unsigned m3  = 0x0F0F0F0FUL;
const unsigned m4  = 0x0000FFFFUL;

int popc(unsigned x) {
  x = x - ((x >> 1) & m1);
  x = x - 3*((x >> 2) & m2);
  x = (x + (x >> 4)) & m3;
  x = x + (x >> 8);
  x = x + (x >> 16);
  return x & m4;
}

/* { dg-final { scan-tree-dump-not "\.POPCOUNT" "optimized" } } */
