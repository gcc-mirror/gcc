/* { dg-options "-msave-restore" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */

/* This test covers a case where we can't (currently) remove the calls to
   the save/restore stubs.  The cast of the return value from BAR requires
   a zero extension between the call to BAR, and the return from FOO, this
   currently prevents the removal of the save/restore calls.  */

typedef unsigned long long u_64;
typedef unsigned int u_32;

extern u_32 bar (u_32 arg);

u_64 foo (u_32 arg)
{
  return (u_64) bar (arg);
}

/* { dg-final { scan-assembler "call\[ \t\]*t0,__riscv_save_0" } } */
/* { dg-final { scan-assembler "tail\[ \t\]*__riscv_restore_0" } } */
