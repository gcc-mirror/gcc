/* { dg-do compile } */
/* The pic register save adds unavoidable stack pointer references.  */
/* { dg-skip-if "" { ia32 && { ! nonpic } } } */
/* These options are selected to ensure 1 word needs to be allocated
   on the stack to maintain alignment for the call.  This should be
   transformed to push+pop.  We also want to force unwind info updates.  */
/* { dg-options "-O2 -mtune-ctrl=single_push,single_pop -fomit-frame-pointer -fasynchronous-unwind-tables" } */
/* { dg-additional-options "-mpreferred-stack-boundary=3" { target ia32 } } */
/* { dg-additional-options "-mpreferred-stack-boundary=4" { target { ! ia32 } } } */
/* ms_abi has reserved stack-region.  */
/* { dg-skip-if "" { x86_64-*-mingw* } } */

extern void g (void);
int
f (void)
{
  g ();
  return 42;
}

/* { dg-final { scan-assembler-not "(sub|add)(l|q)\[\\t \]*\\$\[0-9\]*,\[\\t \]*%\[re\]?sp" } } */
