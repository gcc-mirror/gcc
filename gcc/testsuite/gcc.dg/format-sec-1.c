/* Test for security warning when non-literal format has no arguments.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat -Wformat-security" } */

extern int printf (const char *, ...);

void
foo (char *s)
{
  printf (s); /* { dg-warning "no format arguments" "security warning" } */
}
