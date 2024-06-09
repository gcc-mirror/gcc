/* Make sure that external refences for libcalls are generated even for
   indirect calls.  */

/* { dg-do compile { target { int128 && lp64 } } } */
/* { dg-options "-O2 -mcmodel=large" } */
/* { dg-final { scan-assembler "globl\t__divti3" } } */
/* { dg-skip-if "PR90698" { *-*-darwin* } } */

__int128 a, b; void foo () { a = a / b; }
