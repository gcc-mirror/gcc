/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection -fdump-rtl-pro_and_epilogue -fno-optimize-sibling-calls --param stack-clash-protection-probe-interval=12 --param stack-clash-protection-guard-size=12" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

int f (int *);

int
g (int a)
{
  return f (&a);
}

int f1 (void);
int f2 (int);

int
f3 (void)
{
  return f2 (f1 ());
}


/* If we have caller implicit probes, then we should not need probes in either callee.
   Else callees may need probes, particularly if non-leaf functions require a
   frame/frame pointer.  */
/* { dg-final { scan-rtl-dump-times "Stack clash no probe" 2 "pro_and_epilogue" { target caller_implicit_probes } } } */
/* { dg-final { scan-rtl-dump-times "Stack clash inline probe" 1 "pro_and_epilogue" { target { ! caller_implicit_probes } } } } */
/* { dg-final { scan-rtl-dump-times "Stack clash no probe" 1 "pro_and_epilogue" { target { ! caller_implicit_probes } } } } */

/* Neither of these functions are a nonreturn function.  */
/* { dg-final { scan-rtl-dump-times "Stack clash not noreturn" 2 "pro_and_epilogue" } } */

/* If the callee realigns the stack or has a mandatory frame, then both functions
   have a residual allocation.  Else just g() has a residual allocation.  */
/* { dg-final { scan-rtl-dump-times "Stack clash residual allocation in prologue" 2 "pro_and_epilogue" } } */


/* If the target has frame pointers for non-leafs, then both functions will
   need a frame pointer.  Otherwise neither should.  */
/* { dg-final { scan-rtl-dump-times "Stack clash no frame pointer needed" 2 "pro_and_epilogue" { target { ! frame_pointer_for_non_leaf } } } } */
/* { dg-final { scan-rtl-dump-times "Stack clash frame pointer needed" 2 "pro_and_epilogue" { target { frame_pointer_for_non_leaf } } } } */
/* { dg-final { scan-rtl-dump-times "Stack clash no probe small stack adjustment in prologue" 2 "pro_and_epilogue" { target { aarch64*-*-* } } } } */
