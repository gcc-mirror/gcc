/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16 -fomit-frame-pointer -momit-leaf-frame-pointer -fno-stack-protector" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

void g (volatile int *x) ;
void h (void) __attribute__ ((noreturn));

void
f (void)
{
  volatile int x[16384 + 1000];
  g (x);
  h ();
}

/* { dg-final { scan-assembler-times {str\s+xzr, \[sp, 1024\]} 1 } } */
/* { dg-final { scan-assembler-times {str\s+x30, \[sp\]} 1 } } */

/* SIZE is more than 1 guard-size, two 64k pages used, expect only 1 explicit
   probe at 1024 and one implicit probe due to LR being saved.  Leaf function
   and omitting leaf pointers, normal function call followed by a tail call to
   noreturn which may only omit an epilogue and not a prologue and control flow
   in between.  Checking for LR saving.  */
