/* Test for ICE handling internal formats: bug 20740.  The code did
   not check that, if the required typedef names had been used as
   identifiers, they were defined to suitable types.  Test
   "location_t", not a type.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

int location_t;
void bar (const char *, ...) __attribute__ ((__format__ (__gcc_diag__, 1, 2))); /* { dg-error "'location_t' is not defined as a type" } */
