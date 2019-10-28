/* { dg-options "-Os -msave-restore" } */

/* With -msave-restore in use GCC should be able to remove the calls to the
   save and restore stubs in this case, replacing them with a tail call to
   foo.  */

extern int foo ();

int bar ()
{
  return foo ();
}

/* { dg-final { scan-assembler-not "call\[ \t\]*t0,__riscv_save_0" } } */
/* { dg-final { scan-assembler-not "tail\[ \t\]*__riscv_restore_0" } } */
/* { dg-final { scan-assembler "tail\[ \t\]*foo" } } */
