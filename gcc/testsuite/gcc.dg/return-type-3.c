/* PR optimization/7189
   This was a missing warning caused by a cfg cleanup after sibling
   call optimization.  The return clobber insn was cleaned up and
   the warning was never issued.  */
/* { dg-do compile } */
/* { dg-options "-O -foptimize-sibling-calls -Wreturn-type" } */

extern void foo(void);

int
bar (void)
{
  foo();
} /* { dg-warning "control reaches end of non-void function" "warning for falling off end of non-void function" } */
