/* PR rtl-optimization/102733 */
/* { dg-do compile } */
/* { dg-options "-O1" } */

/* DSE was removing a store to fs:0 (correctly)
   and gs:0 (incorrectly) as DSE didn't take into
   account the address space was different.  */

void test_null_store (void)
{
  int __seg_fs *fs = (int __seg_fs *)0;
  *fs = 1;   

  int __seg_gs *gs = (int __seg_gs *)0;
  *gs = 2;   
  *fs = 3;   
}

/* { dg-final { scan-assembler-times "movl\t" 2 } } */
/* { dg-final { scan-assembler "gs:" } } */
/* { dg-final { scan-assembler "fs:" } } */
