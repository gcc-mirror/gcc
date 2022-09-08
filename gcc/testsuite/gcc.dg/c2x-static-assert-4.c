/* Test C2x static assertions.  static_assert keyword.  Failed assertions.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic" } */

static_assert (0); /* { dg-error "static assertion failed" } */
static_assert (0, "message"); /* { dg-error "message" } */
