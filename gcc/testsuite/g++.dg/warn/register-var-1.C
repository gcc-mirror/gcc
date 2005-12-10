/* PR/18160 */

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */

/* This should yield an error even without -pedantic.  */
/* { dg-options "-ansi" } */

void g(int *);

void f(void) 
{ 
  register int x __asm ("eax");
  g(&x);	/* { dg-error "error: address of explicit register variable" } */
} 
