/* { dg-do run } */
/* { dg-options "-O3 -mhtm -Wa,-march=zEC12 --save-temps" } */

/* __builtin_tbegin has to emit clobbers for all FPRs since the tbegin
   instruction does not automatically preserves them.  If the
   transaction body is fully contained in a function the backend tries
   after reload to get rid of the FPR save/restore operations
   triggered by the clobbers.  This testcase failed since the backend
   was able to get rid of all FPR saves/restores and since these were
   the only stack operations also of the entire stack space.  So even
   the save/restore of the stack pointer was omitted in the end.
   However, since the frame layout has been fixed before, the prologue
   still generated the stack pointer decrement making foo return with
   a modified stack pointer.  */

void abort(void);

void __attribute__((noinline))
foo (int a)
{
  /* This is just to prevent the tbegin code from actually being
     executed.  That way the test may even run on machines prior to
     zEC12.  */
  if (a == 42)
    return;

  if (__builtin_tbegin (0) == 0)
    __builtin_tend ();
}

#ifdef __s390x__
#define GET_STACK_POINTER(SP)			\
  asm volatile ("stg %%r15, %0" : "=QRST" (SP));
#else
#define GET_STACK_POINTER(SP)			\
  asm volatile ("st %%r15, %0" : "=QR" (SP));
#endif

int main(void)
{
  unsigned long new_sp, old_sp;

  GET_STACK_POINTER (old_sp);
  foo(42);
  GET_STACK_POINTER (new_sp);

  if (old_sp != new_sp)
    abort ();

  return 0;
}

/* Make sure no FPR saves/restores are emitted.  */
/* { dg-final { scan-assembler-not "\tstd\t" } } */
/* { dg-final { scan-assembler-not "\tld\t" } } */
