/* N3356 - if declarations.  */
/* PR c/117019 */
/* { dg-do compile } */
/* { dg-options "-std=c2y" } */
/* Test C2Y if declarations.  Invalid usages.  */

void
g (int g)
{
  if (;);	    /* { dg-error "expected" } */
  if (int);	    /* { dg-error "expected|initializer" } */
  if (auto);	    /* { dg-error "expected|initializer" } */
  if (int;);	    /* { dg-error "initializer" } */
  /* { dg-warning "empty" "" { target *-*-* } .-1 } */
  if (auto;);	    /* { dg-error "empty|initializer" } */
  if (int i);	    /* { dg-error "initializer" } */
  if (int i;);	    /* { dg-error "expected" } */
  if (int i = 0;);  /* { dg-error "expected" } */

  if (extern int i = 0);  /* { dg-error "both .extern. and initializer" } */
  if (extern int i);  /* { dg-error "initializer" } */
  if (thread_local int i = 0);  /* { dg-error "function-scope" } */
  if (typedef int i); /* { dg-error "initializer" } */
  if (typedef int i = 0); /* { dg-error "initialized" } */

  if (int i = 2, j = 3);  /* { dg-error "only declare a single object" } */

  if (void (*fp)(int));  /* { dg-error "initializer" } */
  if ([[maybe_unused]] g);  /* { dg-error "expected" } */
  if ([[maybe_unused]] 42);  /* { dg-error "expected" } */
  if ([[maybe_unused]] int);  /* { dg-error "expected|initializer" } */
  if (__attribute__((unused)) g); /* { dg-error "initializer" } */
  if (__attribute__((unused)) 42);  /* { dg-error "expected|initializer" } */
  if (__attribute__((unused)) int);  /* { dg-error "expected|initializer" } */
}
