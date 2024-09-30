/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection -fdump-rtl-pro_and_epilogue -fno-optimize-sibling-calls --param stack-clash-protection-probe-interval=12 --param stack-clash-protection-guard-size=12" } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-skip-if "" { *-*-* } { "-fstack-protector*" } { "" } } */


extern void foo (char *);
extern void bar (void);


/* This is a leaf with a frame that is large enough to require probing with
   a residual allocation, but small enough to probe inline.  */
void
f4 (void)
{
  char buf[4096 + 512];
  asm volatile ("" : : "g" (&buf) : "memory");
}


/* This is a non-leaf with a frame large enough to require probing and
   a residual allocation, but small enough to probe inline.  */
void
f5 (void)
{
  char buf[4096 + 512];
  foo (buf);
}

/* This is a leaf with a frame that is large enough to require probing with
   a loop plus a residual allocation.  */
void
f6 (void)
{
  char buf[4096 * 10 + 512];
  asm volatile ("" : : "g" (&buf) : "memory");
}


/* This is a non-leaf with a frame large enough to require probing with
   a loop plus a residual allocation.  */
void
f7 (void)
{
  char buf[4096 * 10 + 512];
  foo (buf);
}

/* { dg-final { scan-rtl-dump-times "Stack clash inline probes" 2 "pro_and_epilogue" } } */
/* { dg-final { scan-rtl-dump-times "Stack clash probe loop" 2 "pro_and_epilogue" } } */
/* { dg-final { scan-rtl-dump-times "Stack clash residual allocation in prologue" 4 "pro_and_epilogue" { target { ! riscv*-*-* } } } } */
/* { dg-final { scan-rtl-dump-times "Stack clash not noreturn" 4 "pro_and_epilogue" } } */

/* { dg-final { scan-rtl-dump-times "Stack clash no frame pointer needed" 4 "pro_and_epilogue" { target { ! frame_pointer_for_non_leaf } } } } */
/* { dg-final { scan-rtl-dump-times "Stack clash no frame pointer needed" 2 "pro_and_epilogue" { target { frame_pointer_for_non_leaf } } } } */
/* { dg-final { scan-rtl-dump-times "Stack clash frame pointer needed" 2 "pro_and_epilogue" { target { frame_pointer_for_non_leaf } } } } */
