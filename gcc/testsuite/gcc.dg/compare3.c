/* Test for a bogus warning on comparison between signed and unsigned.
   This was inspired by code in gcc. */

/* { dg-do compile } */
/* { dg-options "-Wsign-compare" } */

int tf = 1;

void f(int x, unsigned int y)
{
  /* Test comparing conditional expressions containing truth values.
     This can occur explicitly, or e.g. when (foo?2:(bar?1:0)) is
     optimized into (foo?2:(bar!=0)).  */
  x > (tf?64:(tf!=x)); /* { dg-bogus "changes signedness" "case 1" } */
  y > (tf?64:(tf!=x)); /* { dg-bogus "changes signedness" "case 2" } */
  x > (tf?(tf!=x):64); /* { dg-bogus "changes signedness" "case 3" } */
  y > (tf?(tf!=x):64); /* { dg-bogus "changes signedness" "case 4" } */

  x > (tf?64:(tf==x)); /* { dg-bogus "changes signedness" "case 5" } */
  y > (tf?64:(tf==x)); /* { dg-bogus "changes signedness" "case 6" } */
  x > (tf?(tf==x):64); /* { dg-bogus "changes signedness" "case 7" } */
  y > (tf?(tf==x):64); /* { dg-bogus "changes signedness" "case 8" } */

  x > (tf?64:(tf>x)); /* { dg-bogus "changes signedness" "case 9" } */
  y > (tf?64:(tf>x)); /* { dg-bogus "changes signedness" "case 10" } */
  x > (tf?(tf>x):64); /* { dg-bogus "changes signedness" "case 11" } */
  y > (tf?(tf>x):64); /* { dg-bogus "changes signedness" "case 12" } */

  x < (tf?64:(tf<x)); /* { dg-bogus "changes signedness" "case 13" } */
  y < (tf?64:(tf<x)); /* { dg-bogus "changes signedness" "case 14" } */
  x < (tf?(tf<x):64); /* { dg-bogus "changes signedness" "case 15" } */
  y < (tf?(tf<x):64); /* { dg-bogus "changes signedness" "case 16" } */

  x > (tf?64:(tf>=x)); /* { dg-bogus "changes signedness" "case 17" } */
  y > (tf?64:(tf>=x)); /* { dg-bogus "changes signedness" "case 18" } */
  x > (tf?(tf>=x):64); /* { dg-bogus "changes signedness" "case 19" } */
  y > (tf?(tf>=x):64); /* { dg-bogus "changes signedness" "case 20" } */

  x > (tf?64:(tf<=x)); /* { dg-bogus "changes signedness" "case 21" } */
  y > (tf?64:(tf<=x)); /* { dg-bogus "changes signedness" "case 22" } */
  x > (tf?(tf<=x):64); /* { dg-bogus "changes signedness" "case 23" } */
  y > (tf?(tf<=x):64); /* { dg-bogus "changes signedness" "case 24" } */

  x > (tf?64:(tf&&x)); /* { dg-bogus "changes signedness" "case 25" } */
  y > (tf?64:(tf&&x)); /* { dg-bogus "changes signedness" "case 26" } */
  x > (tf?(tf&&x):64); /* { dg-bogus "changes signedness" "case 27" } */
  y > (tf?(tf&&x):64); /* { dg-bogus "changes signedness" "case 28" } */

  x > (tf?64:(tf||x)); /* { dg-bogus "changes signedness" "case 29" } */
  y > (tf?64:(tf||x)); /* { dg-bogus "changes signedness" "case 30" } */
  x > (tf?(tf||x):64); /* { dg-bogus "changes signedness" "case 31" } */
  y > (tf?(tf||x):64); /* { dg-bogus "changes signedness" "case 32" } */

  x > (tf?64:(!tf)); /* { dg-bogus "changes signedness" "case 33" } */
  y > (tf?64:(!tf)); /* { dg-bogus "changes signedness" "case 34" } */
  x > (tf?(!tf):64); /* { dg-bogus "changes signedness" "case 35" } */
  y > (tf?(!tf):64); /* { dg-bogus "changes signedness" "case 36" } */

}
