/* Test diagnostics for duplicate typedefs.  Basic diagnostics.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

typedef int I; /* { dg-error "error: previous declaration of 'I' was here" } */
typedef int I; /* { dg-error "error: redefinition of typedef 'I'" } */

typedef int I1; /* { dg-error "error: previous declaration of 'I1' was here" } */
typedef long I1; /* { dg-error "error: conflicting types for 'I1'" } */
