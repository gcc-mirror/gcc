/* Test variadic functions with no named parameters do not accept GNU
   attributes before '...'.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu2x" } */

int f (__attribute__(()) ...); /* { dg-error "expected" } */
int g (int (__attribute__(()) ...)); /* { dg-error "expected" } */
int h (__attribute__(()) ...) { return 0; } /* { dg-error "expected" } */
