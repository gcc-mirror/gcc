/* Test GNU parameter forward declarations.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

/* Valid uses.  */
int f1(int a; int a);
int f2(int a; int a) { return 0; }
int f3(int a; int a; int a);
int f4(int a; int a; int a) { return 0; }
int f5(int a; int (*x)[a], int a);
int f6(int a; int (*x)[a], int a) { return 0; }
int f7(int a; int (*x)[a]; int (*y)[x[1][2]], int (*x)[a], int a);
int f8(int a; int (*x)[a]; int (*y)[x[1][2]], int (*x)[a], int a) { return 0; }

/* Strange uses accepted by GCC 4.0 but not by 3.4 and probably not
   desirable to accept.  */
int g1(int a;); /* { dg-error "just a forward declaration" "no parms" { xfail *-*-* } } */
int g2(int a; __attribute__((unused))); /* { dg-error "just a forward declaration" "no parms" { xfail *-*-* } } */
int g3(int;); /* { dg-error "just a forward declaration" "no parms" { xfail *-*-* } } */
int g4(int, long;); /* { dg-error "just a forward declaration" "no parms" { xfail *-*-* } } */

/* Invalid uses.  */
int h1(int a; int b); /* { dg-error "just a forward declaration" } */
int h2(int; int b); /* { dg-error "just a forward declaration" } */
int h3(int a; long a); /* { dg-error "conflicting types|previous definition|just a forward declaration" } */
