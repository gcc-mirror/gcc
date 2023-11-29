/* Make sure that external refences for libcalls are generated even for
   indirect calls.  */

/* { dg-do compile } */
/* __int128 is not supported in x86 -m32.  */
/* { dg-skip-if "" { ! { x86_64-*-* && { ! ilp32 } } } } */
/* { dg-options "-O2 -mcmodel=large" { target x86_64-*-* } } */
/* { dg-final { scan-assembler "globl\t__divti3" } } */

__int128 a, b; void foo () { a = a / b; }
