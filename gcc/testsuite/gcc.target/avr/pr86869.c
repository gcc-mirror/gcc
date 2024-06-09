/* { dg-do compile { target { ! avr_tiny } } } */
/* { dg-additional-options "-std=gnu99 -w" } */

struct S {
  char y[2];
};

void foo(const __memx  struct S *s) {
  const char (*p)[2] = &s->y;
}
