/* Test to make sure that invalid attributes aren't translated.
   If error recovery is ever testable then "foobar" should be
   translated.  */
/* { dg-do compile }
   { dg-require-iconv "IBM1047" }
*/
int foo __attribute__ ((walrus)); /* { dg-error "walrus" "ignored" } */
char x[] = "foobar";
