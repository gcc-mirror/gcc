/* Test invalid array sizes at file scope: should not cause ICEs.
   Bugs 25161 and 27020.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

int a;

int b0[a]; /* { dg-error "5:at file scope" } */
int (*b1)[a]; /* { dg-error "7:at file scope" } */
int (*b2())[a]; /* { dg-error "at file scope" } */
struct b3 { int x[a]; }; /* { dg-error "17:at file scope" } */
struct b4 { int (*x)[a]; }; /* { dg-error "19:at file scope" } */
typeof (int [a]) b5; /* { dg-error "at file scope|outside of any function" } */

int c0[(__SIZE_TYPE__)&a]; /* { dg-error "5:at file scope" } */
int (*c1)[(__SIZE_TYPE__)&a]; /* { dg-error "7:at file scope" } */
int (*c2())[(__SIZE_TYPE__)&a]; /* { dg-error "7:at file scope" } */
struct c3 { int x[(__SIZE_TYPE__)&a]; }; /* { dg-error "17:at file scope" } */
struct c4 { int (*x)[(__SIZE_TYPE__)&a]; }; /* { dg-error "19:at file scope" } */
typeof (int [(__SIZE_TYPE__)&a]) c5; /* { dg-error "34:at file scope" } */

int d0[1/0]; /* { dg-error "5:at file scope" } */
/* { dg-warning "9:division by zero" "" { target *-*-* } 23 } */
int (*d1)[1/0]; /* { dg-error "7:at file scope" } */
/* { dg-warning "12:division by zero" "" { target *-*-* } 25 } */
int (*d2())[1/0]; /* { dg-error "7:at file scope" } */
/* { dg-warning "14:division by zero" "" { target *-*-* } 27 } */
struct d3 { int x[1/0]; }; /* { dg-error "17:at file scope" } */
/* { dg-warning "20:division by zero" "" { target *-*-* } 29 } */
struct d4 { int (*x)[1/0]; }; /* { dg-error "19:at file scope" } */
/* { dg-warning "23:division by zero" "" { target *-*-* } 31 } */
typeof (int [1/0]) d5; /* { dg-error "20:at file scope" } */
