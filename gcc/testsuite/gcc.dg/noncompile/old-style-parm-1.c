/* Test that stray semicolon in old-style parameters is not
   accepted.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

void
f(a)
     int a;; /* { dg-error "parse error|syntax error|expected declaration specifiers" } */
{
}
