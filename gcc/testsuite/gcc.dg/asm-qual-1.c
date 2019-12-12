/* Test that qualifiers other than volatile are disallowed on asm.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

void
f (void)
{
  asm volatile ("");

  asm const (""); /* { dg-error {'const' is not a valid 'asm' qualifier} } */

  asm restrict (""); /* { dg-error {'restrict' is not a valid 'asm' qualifier} } */
}
