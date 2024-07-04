/* Test C23 static assertions.  static_assert keyword.  Failed assertions.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic" } */

static_assert (0); /* { dg-error "static assertion failed" } */
static_assert (0, "message"); /* { dg-error "message" } */
