/* PR middle-end/102630 - Spurious -Warray-bounds with named address space
  { dg-do compile }
  { dg-options "-O -Wall" }
  { dg-final { scan-assembler "fs:0" } }
  { dg-final { scan-assembler "gs:0" } } */

void test_fs_null_store (void)
{
  int __seg_fs *fs = (int __seg_fs *)0;
  *fs = 1;
}

void test_gs_null_store (void)
{
  int __seg_gs *gs = (int __seg_gs *)0;
  *gs = 2;
}
