/* Invalid initializers should not receive an "is not constant"
   error.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

int p = c; /* { dg-error "undeclared" } */
struct s { int a; } x = { b }; /* { dg-error "undeclared" } */
