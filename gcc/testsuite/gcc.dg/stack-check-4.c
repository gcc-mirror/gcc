/* On targets where the call instruction is an implicit probe of *sp, we
   elide stack probes as long as the size of the local stack is less than
   PROBE_INTERVAL.

   But if the caller were to transform a tail call into a direct jump
   we do not have that implicit probe.  This normally isn't a problem as
   the caller must not have a local frame for that optimization to apply.

   However, a sufficiently smart compiler could realize that the caller's
   local stack need not be torn down and thus could transform a call into
   a jump if the target is a noreturn function, even if the caller has
   a local frame.

   To guard against that, targets that depend on *sp being probed by the
   call itself must emit a probe if the target function is a noreturn
   function, even if they just allocate a small amount of stack space.

   Rather than try to parse RTL or assembly code, we instead require the
   prologue code to emit information into the dump file that we can
   scan for.   We scan for both the positive and negative cases.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection -fdump-rtl-pro_and_epilogue -fno-optimize-sibling-calls" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

extern void arf (char *);

__attribute__ ((noreturn)) void foo1 ()
{
  char x[10];
  while (1)
    arf (x);
}

void foo2 ()
{
  char x[10];
  arf (x);
}
/* { dg-final { scan-rtl-dump-times "Stack clash noreturn" 1 "pro_and_epilogue" } } */
/* { dg-final { scan-rtl-dump-times "Stack clash not noreturn" 1 "pro_and_epilogue" } } */

