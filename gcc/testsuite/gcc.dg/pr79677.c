/* PR c/79677 */
/* { dg-do compile } */
/* { dg-options "-Wformat -Werror=format-security -Wformat" } */
/* { dg-message "some warnings being treated as errors" "" { target *-*-* } 0 } */

void foo (char *);

int
main ()
{
  char s[10] = "%s";
  foo (s);
  __builtin_printf (s);	/* { dg-error "format not a string literal and no format arguments" } */
  return 0;
}
