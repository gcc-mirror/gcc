/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

__attribute__((no_sanitize("foobar")))
static void
float_cast2 (void)
{ /* { dg-warning "attribute directive ignored" } */
  volatile double d = 300;
  volatile signed char c;
  c = d;
}
