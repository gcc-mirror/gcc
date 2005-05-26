/* A very simple @try-@catch example. */

/* { dg-do compile } */
/* { dg-options "-fobjc-exceptions" } */

int foo(void) {
  @try {
    return 2;
  }
  @catch (id foo) {
    return 1;
  }
  return 0;
}
