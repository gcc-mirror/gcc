/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -Wno-psabi" } */

typedef unsigned int v4ui __attribute__ ((vector_size (16)));

unsigned long
round_up (unsigned long x)
{
  return ((x - 1) | 4095) + 1;
}

unsigned long
round_up_pages (unsigned long x)
{
  return (((x - 1) | 4095) + 1) >> 12;
}

v4ui
round_up_vec (v4ui x)
{
  v4ui one = { 1, 1, 1, 1 };
  v4ui mask = { 15, 15, 15, 15 };
  return ((x - one) | mask) + one;
}

unsigned long
keep (unsigned long x)
{
  return ((x - 1) | 4096) + 1;
}

/* { dg-final { scan-tree-dump-not "\\| 4095" "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\|.*15" "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\| 4096" 1 "optimized" } } */
