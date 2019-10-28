/* { dg-options "-Os -msave-restore" } */

/* With -msave-restore in use it should not be possible to remove the calls
   to the save and restore stubs in this case (in current GCC).  */

enum
  {
   VAL_A,
   VAL_B,
   VAL_C,
   VAL_D
  } a;

extern void other_1 ();
extern void other_2 ();

void func ()
{
  switch (a)
    {
    case VAL_B:
    case VAL_C:
      other_1 ();
    case VAL_D:
      other_2 ();
    }
}

/* { dg-final { scan-assembler "call\[ \t\]*t0,__riscv_save_0" } } */
/* { dg-final { scan-assembler "tail\[ \t\]*__riscv_restore_0" } } */
