/* { dg-do compile } */
/* { dg-options "-O2" } */

extern int dbg_cnt (void);

struct function
{
  unsigned int calls_setjmp:1;
};
extern struct function *cfun;
unsigned char
gate_rtl_cprop (void)
{
  return !(cfun + 0)->calls_setjmp && dbg_cnt ();
}

/* This should be implementable without performing a bitmask as we can
   just use a test imm,mem.  So instructions which load the object from
   memory and mask off bits are unnecessary.  In theory we can just count
   the move-with-extension, and and testb instructions.  There should be
   only one.  */
/* { dg-final { scan-assembler-times "movzbl|and|testb" 1 } } */
