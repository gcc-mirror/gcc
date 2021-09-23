/* Test C2x digit separators.  Invalid usages.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

void
tf (void)
{
  int i;
  i = 1''2; /* { dg-error "adjacent digit separators" } */
  i = 0x'0; /* { dg-error "digit separator after base indicator" } */
  i = 0X'1; /* { dg-error "digit separator after base indicator" } */
  i = 0b'0; /* { dg-error "digit separator after base indicator" } */
  i = 0B'1; /* { dg-error "digit separator after base indicator" } */
  i = 1'u; /* { dg-error "digit separator outside digit sequence" } */
  float f = 1.2e-3'f; /* { dg-error "digit separator outside digit sequence" } */
  i = 1'2'3'; /* { dg-error "12:missing terminating" } */
  ;
  double d;
  d = 1'.2'3e-4; /* { dg-warning "multi-character" } */
  /* { dg-error "expected" "parse error" { target *-*-* } .-1 } */
  d = 1.2''3; /* { dg-error "adjacent digit separators" } */
  d = 1.23e-4''5; /* { dg-error "adjacent digit separators" } */
  d = 1.2'3e-4'5'; /* { dg-error "17:missing terminating" } */
  /* { dg-error "expected" "parse error" { target *-*-* } .-1 } */
}
