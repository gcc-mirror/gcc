/* Test for execution character set encoding errors.
   If we ever get a good way to test error recovery
   the string "foobar" should be translated.  */
/* { dg-do compile } */
/* { dg-require-iconv "IBM1047" } */
asm (not_a_string); /* { dg-error "(parse error|syntax error|expected string literal) before" "not_a_string" } */
char x[] = "foobar";

void foo (void)
{
  char *y;
  asm (not_a_string2); /* { dg-error "(parse error|syntax error|expected string literal) before" "not_a_string" } */

#define FOO "walrus"
  y = FOO;
}
