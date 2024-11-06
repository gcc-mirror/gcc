/* { dg-do compile } */
/* { dg-options "-std=gnu17 -fpermissive" } */

static l; /* { dg-warning "type defaults to" } */

foo (a) /* { dg-warning "return type defaults to" } */
/* { dg-warning "type of .a. defaults to .int." "type" { target *-*-* } .-1 } */
{
  auto p; /* { dg-warning "type defaults to" } */
  typedef bar; /* { dg-warning "type defaults to" } */
}
