/* Test warnings for shadowing in function prototype scope: generally
   useless but of use if the parameter is used within the scope.  Bug
   529.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -Wshadow" } */

int v; /* { dg-message "shadowed declaration" } */
int f1(int v);
int f2(int v, int x[v]); /* { dg-warning "declaration of 'v' shadows a global declaration" } */
int f3(int v, int y[sizeof(v)]); /* { dg-warning "declaration of 'v' shadows a global declaration" } */
int f4(int v) { return 0; } /* { dg-warning "declaration of 'v' shadows a global declaration" } */
int f5(int v, int x[v]) { return 0; } /* { dg-warning "declaration of 'v' shadows a global declaration" } */
int f6(int x) { return 0; }
int f7(v) int v; { return 0; } /* { dg-warning "declaration of 'v' shadows a global declaration" } */
int f8(v, w) int v; int w[v]; { return 0; } /* { dg-warning "declaration of 'v' shadows a global declaration" } */
int f9(x) int x; { return 0; }
int f10(v) { return 0; } /* { dg-warning "declaration of 'v' shadows a global declaration" } */
int f11(int a, int b(int a));
int f12(int a, int b(int a, int x[a])); /* { dg-warning "declaration of 'a' shadows a parameter" } */
/* { dg-message "shadowed declaration" "outer parm" { target *-*-* } 20 } */
