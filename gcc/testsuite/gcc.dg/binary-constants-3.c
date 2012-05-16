/* Test for binary integer constants: -pedantic-errors.  */

/* Origin: Joerg Wunsch <j.gnu@uriah.heep.sax.de>.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors -ftrack-macro-expansion=0" } */

#define FOO 0b1101

int
foo (void)
{
#if FOO /* { dg-error "binary constants are a GCC extension" } */
  return 23;
#endif
  return 0b1101; /* { dg-error "binary constants are a GCC extension" } */
}
