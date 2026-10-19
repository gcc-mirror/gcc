/* Verify proper errors are generated for indirect function calls.  */
/* { dg-do compile } */
/* { dg-options "-O0 -mno-callx" } */

void (*fnp) (void);

void
foo ()
{
  (*fnp) (); /* { dg-error "indirect call in function" } */
}
