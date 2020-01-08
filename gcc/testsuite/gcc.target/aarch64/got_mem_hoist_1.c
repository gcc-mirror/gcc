/* { dg-do compile } */
/* { dg-options "-O2 -fpic -fdump-rtl-loop2_invariant" } */
/* { dg-require-effective-target fpic } */
/* { dg-skip-if "Load/Store hoisted by RTL PRE already" { aarch64*-*-* }  { "-mcmodel=tiny" "-mcmodel=large" } { "" } } */

int bar (int);
int cal (void *);

int
foo (int a, int bound)
{
  int i = 0;
  int sum = 0;

  for (i; i < bound; i++)
    sum = cal (bar);

  return sum;
}

/* The insn which loads function address from GOT table should be moved out
   of the loop.  */
/* { dg-final { scan-rtl-dump "Decided" "loop2_invariant" } } */
