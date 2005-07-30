/* Test GNU parameter forward declarations.  OK with
   -Wredundant-decls.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wredundant-decls" } */

int f1(int a; int a);
int f2(int a; int a) { return 0; }
int f3(int a; int a; int a);
int f4(int a; int a; int a) { return 0; }
