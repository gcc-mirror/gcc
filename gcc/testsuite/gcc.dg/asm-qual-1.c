/* Test that qualifiers other than volatile are disallowed on asm.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

void
f (void)
{
  asm volatile ("");

  asm const (""); /* { dg-warning {'const' is not an asm qualifier} } */

  asm restrict (""); /* { dg-warning {'restrict' is not an asm qualifier} } */
}
