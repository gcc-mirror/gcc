/* Test for a bogus warning on comparison between signed and unsigned.
   This was inspired by code in gcc. */

/* { dg-do compile } */
/* { dg-options "-Wsign-compare" } */

int tf = 1;

void f(int x, unsigned int y)
{
  /* ?: branches are constants.  */
  x > (tf?64:128); /* { dg-bogus "signed and unsigned" "case 1" } */
  y > (tf?64:128); /* { dg-bogus "signed and unsigned" "case 2" } */

  /* ?: branches are (recursively) constants.  */
  x > (tf?64:(tf?128:256)); /* { dg-bogus "signed and unsigned" "case 3" } */
  y > (tf?64:(tf?128:256)); /* { dg-bogus "signed and unsigned" "case 4" } */

  /* ?: branches are signed constants.  */
  x > (tf?64:-1); /* { dg-bogus "signed and unsigned" "case 5" } */
  y > (tf?64:-1); /* { dg-warning "signed and unsigned" "case 6" } */

  /* ?: branches are (recursively) signed constants.  */
  x > (tf?64:(tf?128:-1)); /* { dg-bogus "signed and unsigned" "case 7" } */
  y > (tf?64:(tf?128:-1)); /* { dg-warning "signed and unsigned" "case 8" } */

  /* Statement expression.  */
  x > ({tf; 64;}); /* { dg-bogus "signed and unsigned" "case 9" } */
  y > ({tf; 64;}); /* { dg-bogus "signed and unsigned" "case 10" { xfail *-*-* } } */

  /* Statement expression with recursive ?: .  */
  x > ({tf; tf?64:(tf?128:256);}); /* { dg-bogus "signed and unsigned" "case 11" } */
  y > ({tf; tf?64:(tf?128:256);}); /* { dg-bogus "signed and unsigned" "case 12" { xfail *-*-* } } */

  /* Statement expression with signed ?:.  */
  x > ({tf; tf?64:-1;}); /* { dg-bogus "signed and unsigned" "case 13" } */
  y > ({tf; tf?64:-1;}); /* { dg-warning "signed and unsigned" "case 14" } */

  /* Statement expression with recursive signed ?:.  */
  x > ({tf; tf?64:(tf?128:-1);}); /* { dg-bogus "signed and unsigned" "case 15" } */
  y > ({tf; tf?64:(tf?128:-1);}); /* { dg-warning "signed and unsigned" "case 16" } */

  /* ?: branches are constants.  */
  tf ? x : (tf?64:32); /* { dg-bogus "conditional expression" "case 17" } */
  tf ? y : (tf?64:32); /* { dg-bogus "conditional expression" "case 18" } */

  /* ?: branches are signed constants.  */
  tf ? x : (tf?64:-1); /* { dg-bogus "conditional expression" "case 19" } */
  tf ? y : (tf?64:-1); /* { dg-warning "conditional expression" "case 20" } */

  /* ?: branches are (recursively) constants.  */
  tf ? x : (tf?64:(tf?128:256)); /* { dg-bogus "conditional expression" "case 21" } */
  tf ? y : (tf?64:(tf?128:256)); /* { dg-bogus "conditional expression" "case 22" } */

  /* ?: branches are (recursively) signed constants.  */
  tf ? x : (tf?64:(tf?128:-1)); /* { dg-bogus "conditional expression" "case 23" } */
  tf ? y : (tf?64:(tf?128:-1)); /* { dg-warning "conditional expression" "case 24" } */
}
