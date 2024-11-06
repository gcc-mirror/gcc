/* { dg-do compile } */
/* { dg-options "-std=gnu17 -Wno-implicit -Wimplicit-int" } */

static l; /* { dg-error "type defaults to" } */

foo (a) /* { dg-error "return type defaults to" } */
/* { dg-error "type of .a. defaults to .int." "type" { target *-*-* } .-1 } */
{
  auto p; /* { dg-error "type defaults to" } */
  typedef bar; /* { dg-error "type defaults to" } */
}
