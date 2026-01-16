/* Test hierarchical discriminators for loop versioning pass.
   { dg-do compile }
   { dg-options "-O3 -g -fdump-tree-lversion-details" }
   { dg-require-effective-target size32plus }  */

void
test_loop_versioning (double *x, int stepx, int n)
{
  for (int i = 0; i < n; ++i)
    x[stepx * i] = 100;
}

/* Loop versioning creates two versions of the loop, each should get a distinct
   copyid in the hierarchical discriminator format: [Base:8][Multiplicity:7][CopyID:11][Unused:6].
   The exact copyid values depend on what other passes have run before, but both
   loop versions should have non-zero discriminators to distinguish them for AutoFDO. */

/* Check that loop versioning occurred and discriminators are present on the loop body. */
/* { dg-final { scan-tree-dump "versioned this loop for when certain strides are 1" "lversion" } } */
/* { dg-final { scan-assembler "\\.loc 1 (9|10) \[0-9\]+ is_stmt 0 discriminator \[0-9\]+" } } */
