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

/* Expected discriminators from the assembly (hierarchical format: [Base:8][Multiplicity:7][CopyID:11][Unused:6]):
   
   Loop versioning creates two versions:
   1. Optimized loop (for stride == 1):
      - copyid = allocate_copyid_base(loc, 1) = 1 (first allocation)
      - multiplicity = 0 (no vectorization in this pass)
      - Discriminator = base:0 | (0<<8) | (1<<15) = 0 | 0 | 32768 = 32768
   
   2. Unoptimized loop (for stride != 1):
      - copyid = allocate_copyid_base(loc, 1) = 2 (second allocation at same location)
      - multiplicity = 0
      - Discriminator = base:0 | (0<<8) | (2<<15) = 0 | 0 | 65536 = 65536
*/

/* { dg-final { scan-tree-dump "versioned this loop for when certain strides are 1" "lversion" } } */
/* { dg-final { scan-assembler "\\.loc 1 14 \[0-9\]+ is_stmt 0 discriminator \[0-9\]+" } } */
