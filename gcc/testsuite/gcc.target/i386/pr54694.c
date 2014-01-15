/* { dg-do compile } */
/* { dg-options "-O" } */

register void *hfp __asm__("%ebp");	/* { dg-message "note: for" } */

extern void g(void *);

void f(int x)			/* { dg-error "frame pointer required" } */
{
  g(__builtin_alloca(x));
}
