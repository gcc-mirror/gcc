/* Test that qualifiers other than volatile are ignored on asm.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

void
f (void)
{
  asm volatile ("");
  asm const (""); /* { dg-warning "const qualifier ignored on asm" } */
  asm restrict (""); /* { dg-warning "restrict qualifier ignored on asm" } */
}
