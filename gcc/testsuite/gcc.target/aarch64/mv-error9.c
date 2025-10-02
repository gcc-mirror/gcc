/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("dotprod"))) int
foo (); /* { dg-message "previous declaration of .foo \\\[\\\[target_version\\(.dotprod.\\)\\\]\\\]. with type .int\\(void\\)." } */

int
bar ()
{
  return foo (); /* { dg-error "implicit declaration of function .foo." } */
}
