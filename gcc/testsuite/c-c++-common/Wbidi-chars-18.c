/* PR preprocessor/104030 */
/* { dg-do compile } */
/* By default, don't warn about UCNs.  */

const char *
fn ()
{
  const char *aText = "\u202D" "abc";
/* { dg-bogus "unpaired" "" { target *-*-* } .-1 } */
  return aText;
}
