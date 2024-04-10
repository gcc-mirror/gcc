/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection -mtune=generic -fomit-frame-pointer -mnoreturn-no-callee-saved-registers" } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-skip-if "" { *-*-* } { "-fstack-protector*" } { "" } } */



int x0, x1;
void f1 (void);
void f2 (int, int);

__attribute__ ((noreturn))
void
f3 (void)
{ 
  int y0 = x0;
  int y1 = x1;
  f1 ();
  f2 (y0, y1);
  while (1);
}

/* Verify no explicit probes.  */
/* { dg-final { scan-assembler-not "or\[ql\]" } } */

/* y0/y1 are live across the call and thus must be allocated
   into either a stack slot or callee saved register.  The former
   would be rather dumb.  So assume it does not happen.

   So search for a push/pop sequence for stack probe and 2 argument
   pushes on ia32.  There is no need to save and restore the PIC
   register on ia32 for a noreturn function.  */
/* { dg-final { scan-assembler-times "push\[ql\]" 1 { target { ! ia32 } } } }  */
/* { dg-final { scan-assembler-times "push\[ql\]" 3 { target ia32 } } }  */
/* { dg-final { scan-assembler-not "pop" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pop" 1 { target ia32 } } } */
