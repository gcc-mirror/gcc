/* Test for labels in statement expressions: bugs 772 and 17913.
   Test the particular case of bug 17913.  */

/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void f(void) { 1 ? 1 : ({ a : 1; 1; }); goto a; } /* { dg-error "jump into statement expression" } */

/* Match extra informative notes.  */
/* { dg-message "note: label '\[^\n'\]*' defined here" "note: expected" { target *-*-* } 0 } */
