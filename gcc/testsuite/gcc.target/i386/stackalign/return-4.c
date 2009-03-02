/* { dg-do compile } */
/* { dg-options "-mpreferred-stack-boundary=4" } */
/* { dg-final { scan-assembler-not "and\[lq\]?\[^\\n\]*-64,\[^\\n\]*sp" } } */

/* This compile only test is to detect an assertion failure in stack branch
   development.  */
typedef int aligned __attribute__((aligned(64)));

aligned
foo (void) { }
