/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("dotprod"))) int
foo ();

int
bar ()
{
  return foo (); /* { dg-error "no default version in scope" } */
}
