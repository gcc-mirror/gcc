/* Some incompatible external linkage declarations were not diagnosed.
   Bug 21342.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

extern int a[];
void f(void) { extern int a[10]; } /* { dg-error "error: previous declaration of 'a' was here" } */
extern int a[5]; /* { dg-error "error: conflicting types for 'a'" } */
