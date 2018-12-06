/* Test that qualifiers other than volatile are disallowed on asm.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

void
f (void)
{
  asm volatile ("");

  asm const (""); /* { dg-error {expected '\(' before 'const'} } */
  /* { dg-error {expected identifier} {} {target *-*-*} .-1 } */

  asm restrict (""); /* { dg-error {expected '\(' before 'restrict'} } */
  /* { dg-error {expected identifier} {} {target *-*-*} .-1 } */
}
