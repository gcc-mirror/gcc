/* { dg-do compile { target { ! x32 } } } */
/* The pic register save adds unavoidable stack pointer references.  */
/* { dg-skip-if "" { ia32 && { ! nonpic } } } */
/* These options are selected to ensure 1 word needs to be allocated
   on the stack to maintain alignment for the call.  This should be
   transformed to push+pop.  We also want to force unwind info updates.  */
/* { dg-options "-Os -fomit-frame-pointer -fasynchronous-unwind-tables" } */
/* { dg-additional-options "-mno-red-zone" { target { ! ia32 } } } */
/* { dg-additional-options "-mpreferred-stack-boundary=3" { target ia32 } } */
/* ms_abi has reserved stack-region.  */
/* { dg-skip-if "" { x86_64-*-mingw* } } */
void f();
void g() { f(); f(); }

/* Both stack allocate and deallocate should be converted to push/pop.  */
/* { dg-final { scan-assembler-not "sp" } } */
