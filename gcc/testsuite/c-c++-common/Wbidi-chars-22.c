/* PR preprocessor/104030 */
/* { dg-do compile } */
/* { dg-options "-Wbidi-chars=none,ucn" } */

const char *
fn ()
{
  const char *aText = "\u202D" "abc";
/* { dg-bogus "" "" { target *-*-* } .-1 } */
  return aText;
}
