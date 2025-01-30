/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized --param logical-op-non-short-circuit=1" } */

typedef unsigned long int uint64_t;

int cmp1(int d1, int d2) {
  if ((d1 ^ d2) == 0xabcd || d1 != d2)
    return 0;
  return 1;
}

int cmp2(int d1, int d2) {
  if (d1 != d2 || (d1 ^ d2) == 0xabcd)
    return 0;
  return 1;
}

int cmp3(int d1, int d2) {
  if (0xabcd > (d2 ^ d1) || d2 != d1)
    return 0;
  return 1;
}

int cmp4(int d1, int d2) {
  if (d2 != d1 || 0xabcd > (d2 ^ d1))
    return 0;
  return 1;
}

int cmp1_64(uint64_t d1, uint64_t d2) {
  if ((d1 ^ d2) == 0xabcd || d1 != d2)
    return 0;
  return 1;
}

int cmp2_64(uint64_t d1, uint64_t d2) {
  if (d1 != d2 || (d1 ^ d2) == 0xabcd)
    return 0;
  return 1;
}

int cmp3_64(uint64_t d1, uint64_t d2) {
  if (0xabcd > (d2 ^ d1) || d2 != d1)
    return 0;
  return 1;
}

int cmp4_64(uint64_t d1, uint64_t d2) {
  if (d2 != d1 || 0xabcd > (d2 ^ d1))
    return 0;
  return 1;
}

/* The if should be removed, so the condition should not exist */
/* { dg-final { scan-tree-dump-not "d1_\[0-9\]+.D. \\^ d2_\[0-9\]+.D." "optimized" } } */
