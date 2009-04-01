/* PR c/37772 */
/* { dg-do compile } */
/* { dg-options "" } */

void
foo ()
{
  int i;
  asm ();		  /* { dg-error "expected string literal before" } */
  asm (1);		  /* { dg-error "expected string literal before" } */
  asm (int);		  /* { dg-error "expected string literal before" } */
  asm (: "=r" (i));	  /* { dg-error "expected string literal before" } */
  asm (1 : "=r" (i));	  /* { dg-error "expected string literal before" } */
  asm (int : "=r" (i));	  /* { dg-error "expected string literal before" } */
  asm (: : "r" (i));	  /* { dg-error "expected string literal before" } */
  asm (1 : : "r" (i));	  /* { dg-error "expected string literal before" } */
  asm (int : : "r" (i));  /* { dg-error "expected string literal before" } */
  asm (: : : "memory");	  /* { dg-error "expected string literal before" } */
  asm (1 : : : "memory"); /* { dg-error "expected string literal before" } */
}
