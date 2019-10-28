/* { dg-options "-Os -msave-restore" } */

/* With -msave-restore in use it should not be possible to remove the calls
   to the save and restore stubs in this case (in current GCC).  */

extern void fn2 ();

volatile int a = 0;

int
fn1 ()
{
  fn2 ();

  while (a)
    ;

  return 0;
}

/* { dg-final { scan-assembler "call\[ \t\]*t0,__riscv_save_0" } } */
/* { dg-final { scan-assembler "tail\[ \t\]*__riscv_restore_0" } } */
