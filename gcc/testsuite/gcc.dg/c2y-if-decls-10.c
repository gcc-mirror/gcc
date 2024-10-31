/* N3356 - if declarations.  */
/* PR c/117019 */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */
/* Test C2Y if declarations.  Invalid usages.  */

void
g (int g)
{
  switch (;);	    /* { dg-error "expected" } */
  switch (int);	    /* { dg-error "expected identifier" } */
  /* { dg-error "declaration" "" { target *-*-* } .-1 } */
  switch (auto);    /* { dg-error "expected identifier" } */
  /* { dg-error "declaration" "" { target *-*-* } .-1 } */
  switch (int;);    /* { dg-error "declaration" } */
  switch (auto;);	    /* { dg-error "empty|initializer" } */
  switch (int i);	    /* { dg-error "initializer" } */
  switch (int i;);	    /* { dg-error "expected" } */
  switch (int i = 0;);  /* { dg-error "expected" } */

  switch (extern int i = 0);  /* { dg-error "both .extern. and initializer" } */
  switch (extern int i);  /* { dg-error "initializer" } */
  switch (thread_local int i = 0); /* { dg-error "function-scope" } */
  switch (typedef int i); /* { dg-error "initializer" } */
  switch (typedef int i = 0); /* { dg-error "initialized" } */

  switch (int i = 2, j = 3);  /* { dg-error "only declare a single object" } */

  switch (void (*fp)(int));  /* { dg-error "initializer" } */
  switch ([[maybe_unused]] g);  /* { dg-error "expected" } */
  switch ([[maybe_unused]] 42);  /* { dg-error "expected" } */
  switch ([[maybe_unused]] int);  /* { dg-error "expected|initializer" } */
  switch (__attribute__((unused)) g); /* { dg-error "initializer" } */
  switch (__attribute__((unused)) 42);  /* { dg-error "expected|initializer" } */
  switch (__attribute__((unused)) int);  /* { dg-error "expected|initializer" } */

  switch (int arr[] = { 1 });  /* { dg-error "switch quantity not an integer" } */
}
