/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection -mtune=generic -fomit-frame-pointer" } */
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

/* We also want to verify we did not use a push/pop sequence
   to probe *sp as the callee register saves are sufficient
   to probe *sp.

   y0/y1 are live across the call and thus must be allocated
   into either a stack slot or callee saved register.  The former
   would be rather dumb.  So assume it does not happen.

   So search for two/four pushes for the callee register saves/argument
   pushes and no pops (since the function has no reachable epilogue).  */
/* { dg-final { scan-assembler-times "push\[ql\]" 2 { target { ! ia32 } } } }  */
/* { dg-final { scan-assembler-times "push\[ql\]" 4 { target { ia32 } } } }  */
/* { dg-final { scan-assembler-not "pop" } } */

