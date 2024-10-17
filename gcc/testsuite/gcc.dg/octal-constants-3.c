/* Test for octal integer constants: -pedantic-errors.  */

/* Origin: Joerg Wunsch <j.gnu@uriah.heep.sax.de>.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors -ftrack-macro-expansion=0" } */

#define FOO 0o1307

int
foo (void)
{
#if FOO /* { dg-error "'0o' prefixed constants are a C2Y feature or GCC extension" } */
  return 23;
#endif
  return 0o1307; /* { dg-error "'0o' prefixed constants are a C2Y feature or GCC extension" } */
}
