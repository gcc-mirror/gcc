/* Test for non-integer enum values.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

enum e { E, F };
enum e2 {
  E1 = (void *)4, /* { dg-error "enumerator value for 'E1' is not an integer constant" } */
  E2 = (enum e)F,
  E3 = (_Bool)1
};
