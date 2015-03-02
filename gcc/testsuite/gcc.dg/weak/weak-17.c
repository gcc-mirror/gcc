/* { dg-do compile } */
/* { dg-require-weak "" } */
#pragma weak int = foo

/* { dg-warning "declaration of" "weak" { target *-*-* } 0 } */
