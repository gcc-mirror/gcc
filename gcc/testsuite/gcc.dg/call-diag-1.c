/* The warning for calling through a non-compatible type must not
   disable the normal diagnostics from comparing the argument list
   against the type of the called expression.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

void foo (void);
void bar (void) { ((long (*)(int))foo) (); } /* { dg-error "too few arguments to function" } */
