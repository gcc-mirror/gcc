/* PR c++/4872 */
/* { dg-do compile } */
/* { dg-options "-Wreturn-type" } */

static inline int f() {}     /* { dg-warning "return" "missing return" } */
