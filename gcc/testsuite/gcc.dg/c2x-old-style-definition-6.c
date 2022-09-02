/* Test old-style function definitions not in C2x: () gives a type with
   a prototype for all declarations.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x" } */

void f1 (); /* { dg-message "declared here" } */

/* Prototyped function returning a pointer to a function with no arguments.  */
void (*f2 (void))() { return f1; }

void
g (void)
{
  f1 (1); /* { dg-error "too many arguments" } */
  f2 () (1); /* { dg-error "too many arguments" } */
}
