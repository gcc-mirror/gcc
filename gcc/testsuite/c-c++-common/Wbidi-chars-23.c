/* PR preprocessor/104030 */
/* { dg-do compile } */
/* { dg-options "-Wbidi-chars=ucn" } */

const char *
fn ()
{
  const char *aText = "\u202D" "abc";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  return aText;
}
