/* Test __attribute__((unavailable)). */
/* { dg-do compile } */
/* { dg-options "" } */

void func(void);
void func(void) __attribute__((unavailable));

void f(void) {
  func(); /* { dg-error "'func' is unavailable" } */
}
