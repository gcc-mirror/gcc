/* Test for a bogus warning on comparison between signed and unsigned.
   This was inspired by code in gcc. */

/* { dg-do compile } */
/* { dg-options "-Wsign-compare" } */

int tf = 1;

void f(int x, unsigned int y)
{
  /* ?: branches are constants.  */
  x > (tf?64:128); /* { dg-bogus "changes signedness" "case 1" } */
  y > (tf?64:128); /* { dg-bogus "changes signedness" "case 2" } */

  /* ?: branches are (recursively) constants.  */
  x > (tf?64:(tf?128:256)); /* { dg-bogus "changes signedness" "case 3" } */
  y > (tf?64:(tf?128:256)); /* { dg-bogus "changes signedness" "case 4" } */

  /* ?: branches are signed constants.  */
  x > (tf?64:-1); /* { dg-bogus "changes signedness" "case 5" } */
  y > (tf?64:-1); /* { dg-warning "different signedness" "case 6" } */

  /* ?: branches are (recursively) signed constants.  */
  x > (tf?64:(tf?128:-1)); /* { dg-bogus "changes signedness" "case 7" } */
  y > (tf?64:(tf?128:-1)); /* { dg-warning "different signedness" "case 8" } */

  /* Statement expression.  */
  x > ({tf; 64;}); /* { dg-bogus "changes signedness" "case 9" } */
  y > ({tf; 64;}); /* { dg-bogus "changes signedness" "case 10" } */

  /* Statement expression with recursive ?: .  */
  x > ({tf; tf?64:(tf?128:256);}); /* { dg-bogus "changes signedness" "case 11" } */
  y > ({tf; tf?64:(tf?128:256);}); /* { dg-bogus "changes signedness" "case 12" } */

  /* Statement expression with signed ?:.  */
  x > ({tf; tf?64:-1;}); /* { dg-bogus "changes signedness" "case 13" } */
  y > ({tf; tf?64:-1;}); /* { dg-warning "different signedness" "case 14" } */

  /* Statement expression with recursive signed ?:.  */
  x > ({tf; tf?64:(tf?128:-1);}); /* { dg-bogus "changes signedness" "case 15" } */
  y > ({tf; tf?64:(tf?128:-1);}); /* { dg-warning "different signedness" "case 16" } */

  /* ?: branches are constants.  */
  tf ? x : (tf?64:32); /* { dg-bogus "changes signedness" "case 17" } */
  tf ? y : (tf?64:32); /* { dg-bogus "changes signedness" "case 18" } */

  /* ?: branches are signed constants.  */
  tf ? x : (tf?64:-1); /* { dg-bogus "changes signedness" "case 19" } */
  tf ? y : (tf?64:-1); /* { dg-warning "changes signedness" "case 20" } */

  /* ?: branches are (recursively) constants.  */
  tf ? x : (tf?64:(tf?128:256)); /* { dg-bogus "changes signedness" "case 21" } */
  tf ? y : (tf?64:(tf?128:256)); /* { dg-bogus "changes signedness" "case 22" } */

  /* ?: branches are (recursively) signed constants.  */
  tf ? x : (tf?64:(tf?128:-1)); /* { dg-bogus "changes signedness" "case 23" } */
  tf ? y : (tf?64:(tf?128:-1)); /* { dg-warning "changes signedness" "case 24" } */
}
