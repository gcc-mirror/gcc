/* This token is long enough to require an ad-hoc location. Make sure that
   the macro trace still prints properly.  */
#define X "0123456789012345678901234567689" /* { dg-error {expected .* before string constant} } */
X /* { dg-note {in expansion of macro 'X'} } */
