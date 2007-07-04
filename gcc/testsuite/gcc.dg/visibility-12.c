/* Test that dllimport'd functions have default visibility.  */
/* { dg-require-visibility "" } */
/* { dg-require-dll "" } */
/* { dg-options "-fvisibility=hidden" } */
/* { dg-final { scan-not-hidden "f1" } } */

extern void  __attribute__((dllimport)) f1();
void f2() {
  f1();
}
