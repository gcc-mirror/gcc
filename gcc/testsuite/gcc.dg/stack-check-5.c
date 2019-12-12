/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection -fdump-rtl-pro_and_epilogue -fno-optimize-sibling-calls --param stack-clash-protection-probe-interval=12 --param stack-clash-protection-guard-size=12" } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-skip-if "" { *-*-* } { "-fstack-protector*" } { "" } } */


/* Otherwise the S/390 back-end might save the stack pointer in f2 ()
   into an FPR.  */
/* { dg-additional-options "-msoft-float" { target { s390x-*-* } } } */

extern void foo (char *);
extern void bar (void);

/* This function allocates no local stack and is a leaf.  It should have no
   probes on any target and should not require a frame pointer.  */
int
f0 (int x, int y)
{
  asm volatile ("" : : : "memory");  
  return x + y;
}

/* This function allocates no local stack, but is not a leaf.  Ideally it
   should not need probing and no frame pointer.  */
int
f1 (int x, int y)
{
  asm volatile ("" : : : "memory");  
  bar ();
}

/* This is a leaf with a small frame.  On targets with implicit probes in
   the caller, this should not need probing.  On targets with no implicit
   probes in the caller, it may require probes.  Ideally it should need no
   frame pointer.  */
void
f2 (void)
{
  char buf[512];
  asm volatile ("" : : "g" (&buf) : "memory");
}

/* This is a non-leaf with a small frame.  On targets with implicit probes in
   the caller, this should not need probing.  On targets with no implicit
   probes in the caller, it may require probes.  It should need no frame
   pointer.  */
void
f3 (void)
{
  char buf[512];
  foo (buf);
}

/* If we have caller implicit probes, then we should not need probes.
   Else callees may need probes, particularly if non-leaf functions require a
   frame/frame pointer.  */
/* { dg-final { scan-rtl-dump-times "Stack clash no probe" 4 "pro_and_epilogue" { target caller_implicit_probes } } } */
/* { dg-final { scan-rtl-dump-times "Stack clash no probe" 2 "pro_and_epilogue" { target { ! caller_implicit_probes } } } } */
/* { dg-final { scan-rtl-dump-times "Stack clash inline probes " 2 "pro_and_epilogue" { target { ! caller_implicit_probes } } } } */

/* None of these functions are marked with the noreturn attribute.  */
/* { dg-final { scan-rtl-dump-times "Stack clash not noreturn" 4 "pro_and_epilogue" } } */

/* Two functions are leafs, two are not.  Verify the target identified them
   appropriately.  */
/* { dg-final { scan-rtl-dump-times "Stack clash no frame pointer needed" 4 "pro_and_epilogue" { target { ! frame_pointer_for_non_leaf } } } } */
/* { dg-final { scan-rtl-dump-times "Stack clash no frame pointer needed" 2 "pro_and_epilogue" { target { frame_pointer_for_non_leaf } } } } */
/* { dg-final { scan-rtl-dump-times "Stack clash frame pointer needed" 2 "pro_and_epilogue" { target { frame_pointer_for_non_leaf } } } } */
/* AArch64 won't require a probe here due to the allocation amount being less than 1KB.  */
/* { dg-final { scan-rtl-dump-times "Stack clash no probe small stack adjustment in prologue" 3 "pro_and_epilogue" { target { aarch64*-*-* } } } } */
/* { dg-final { scan-rtl-dump-times "Stack clash no probe no stack adjustment in prologue" 1 "pro_and_epilogue" { target { aarch64*-*-* } } } } */

/* We have selected the size of the array in f2/f3 to be large enough
   to not live in the red zone on targets that support it.

   That allows simplification of this test considerably.
   f1() should not require any allocations, thus no residuals.
   All the rest of the functions require some kind of allocation,
   either for the saved fp/rp or the array.  */
/* { dg-final { scan-rtl-dump-times "Stack clash no residual allocation in prologue" 1 "pro_and_epilogue" } } */
/* { dg-final { scan-rtl-dump-times "Stack clash residual allocation in prologue" 3 "pro_and_epilogue" } } */
