/* PR preprocessor/104030 */
/* { dg-do compile } */
/* { dg-options "-Wbidi-chars=any" } */

const char *
fn ()
{
  const char *aText = "\u202D" "abc";
/* { dg-bogus "U\\+202D" "" { target *-*-* } .-1 } */
  return aText;
}
