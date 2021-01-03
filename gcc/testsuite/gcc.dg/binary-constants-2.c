/* Test for binary integer constants: -pedantic warnings.  */

/* Origin: Joerg Wunsch <j.gnu@uriah.heep.sax.de>.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic -ftrack-macro-expansion=0" } */

#define FOO 0b1101

int
foo (void)
{
#if FOO /* { dg-warning "binary constants are a C2X feature or GCC extension" } */
  return 23;
#endif
  return 0b1101; /* { dg-warning "binary constants are a C2X feature or GCC extension" } */
}
