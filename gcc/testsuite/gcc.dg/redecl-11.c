/* Some incompatible external linkage declarations were not diagnosed.
   Bug 21342.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

int f(int (*)[]);
void g() { int f(int (*)[2]); } /* { dg-message "note: previous declaration of 'f' was here" } */
int f(int (*)[3]); /* { dg-error "conflicting types for 'f'" } */
