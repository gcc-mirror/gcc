/* Test hierarchical discriminators for GIMPLE partial unroll
   via unroll-and-jam (tree_unroll_loop -> tree_transform_and_unroll_loop).
   { dg-do compile }
   { dg-options "-O3 -g -floop-unroll-and-jam -fno-tree-vectorize --param unroll-jam-min-percent=0 -fdump-tree-unrolljam-details" } */
/* { dg-require-effective-target int32plus } */

unsigned int aa[16][1024];

void
test_unroll_jam (unsigned long n, unsigned long m)
{
  unsigned i, j;
  for (i = 1; i < m; i++)
    for (j = 1; j < n; j++)
      aa[i][j] = aa[i - 1][j] * aa[i - 1][j] / 2;
}

/* { dg-final { scan-tree-dump "applying unroll and jam" "unrolljam" } } */
/* { dg-final { scan-assembler "discriminator (\[1-9\]\[0-9\]*|0x\[1-9a-fA-F\]\[0-9a-fA-F\]*)" { target gas } } } */
