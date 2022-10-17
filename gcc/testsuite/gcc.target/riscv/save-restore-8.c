/* { dg-options "-msave-restore" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

/* As a leaf function this should never have the calls to the save and
   restore stubs added, but lets check anyway.  */

int func ()
{
  return 3;
}

/* { dg-final { scan-assembler-not "call\[ \t\]*t0,__riscv_save_0" } } */
/* { dg-final { scan-assembler-not "tail\[ \t\]*__riscv_restore_0" } } */
