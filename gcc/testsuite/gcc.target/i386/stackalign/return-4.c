/* { dg-do compile } */
/* { dg-options "-mpreferred-stack-boundary=4 -O" } */
/* { dg-final { scan-assembler-not "and\[lq\]?\[^\\n\]*-64,\[^\\n\]*sp" } } */
/* We only guarantee we won't generate the stack alignment when
   optimizing.  When not optimizing, the return value will be assigned
   to a pseudo with the specified alignment, which in turn will force
   stack alignment since the pseudo might have to be spilled.  Without
   optimization, we wouldn't compute the actual stack requirements
   after register allocation and reload, and just use the conservative
   estimate.  */

/* This compile only test is to detect an assertion failure in stack branch
   development.  */
typedef int aligned __attribute__((aligned(64)));

aligned
foo (void) { }
