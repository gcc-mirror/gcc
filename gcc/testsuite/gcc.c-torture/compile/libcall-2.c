/* Make sure that external refences for libcalls are generated even for
   indirect calls.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mcmodel=large" { target x86_64-*-* } } */
/* { dg-final { scan-assembler "globl\t__divti3" } } */

__int128 a, b; void foo () { a = a / b; }
