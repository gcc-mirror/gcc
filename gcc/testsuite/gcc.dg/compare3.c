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
  x > (tf?64:(tf!=x)); /* { dg-bogus "signed and unsigned" "case 1" } */
  y > (tf?64:(tf!=x)); /* { dg-bogus "signed and unsigned" "case 2" } */
  x > (tf?(tf!=x):64); /* { dg-bogus "signed and unsigned" "case 3" } */
  y > (tf?(tf!=x):64); /* { dg-bogus "signed and unsigned" "case 4" } */

  x > (tf?64:(tf==x)); /* { dg-bogus "signed and unsigned" "case 5" } */
  y > (tf?64:(tf==x)); /* { dg-bogus "signed and unsigned" "case 6" } */
  x > (tf?(tf==x):64); /* { dg-bogus "signed and unsigned" "case 7" } */
  y > (tf?(tf==x):64); /* { dg-bogus "signed and unsigned" "case 8" } */

  x > (tf?64:(tf>x)); /* { dg-bogus "signed and unsigned" "case 9" } */
  y > (tf?64:(tf>x)); /* { dg-bogus "signed and unsigned" "case 10" } */
  x > (tf?(tf>x):64); /* { dg-bogus "signed and unsigned" "case 11" } */
  y > (tf?(tf>x):64); /* { dg-bogus "signed and unsigned" "case 12" } */

  x < (tf?64:(tf<x)); /* { dg-bogus "signed and unsigned" "case 13" } */
  y < (tf?64:(tf<x)); /* { dg-bogus "signed and unsigned" "case 14" } */
  x < (tf?(tf<x):64); /* { dg-bogus "signed and unsigned" "case 15" } */
  y < (tf?(tf<x):64); /* { dg-bogus "signed and unsigned" "case 16" } */

  x > (tf?64:(tf>=x)); /* { dg-bogus "signed and unsigned" "case 17" } */
  y > (tf?64:(tf>=x)); /* { dg-bogus "signed and unsigned" "case 18" } */
  x > (tf?(tf>=x):64); /* { dg-bogus "signed and unsigned" "case 19" } */
  y > (tf?(tf>=x):64); /* { dg-bogus "signed and unsigned" "case 20" } */

  x > (tf?64:(tf<=x)); /* { dg-bogus "signed and unsigned" "case 21" } */
  y > (tf?64:(tf<=x)); /* { dg-bogus "signed and unsigned" "case 22" } */
  x > (tf?(tf<=x):64); /* { dg-bogus "signed and unsigned" "case 23" } */
  y > (tf?(tf<=x):64); /* { dg-bogus "signed and unsigned" "case 24" } */

  x > (tf?64:(tf&&x)); /* { dg-bogus "signed and unsigned" "case 25" } */
  y > (tf?64:(tf&&x)); /* { dg-bogus "signed and unsigned" "case 26" } */
  x > (tf?(tf&&x):64); /* { dg-bogus "signed and unsigned" "case 27" } */
  y > (tf?(tf&&x):64); /* { dg-bogus "signed and unsigned" "case 28" } */

  x > (tf?64:(tf||x)); /* { dg-bogus "signed and unsigned" "case 29" } */
  y > (tf?64:(tf||x)); /* { dg-bogus "signed and unsigned" "case 30" } */
  x > (tf?(tf||x):64); /* { dg-bogus "signed and unsigned" "case 31" } */
  y > (tf?(tf||x):64); /* { dg-bogus "signed and unsigned" "case 32" } */

  x > (tf?64:(!tf)); /* { dg-bogus "signed and unsigned" "case 33" } */
  y > (tf?64:(!tf)); /* { dg-bogus "signed and unsigned" "case 34" } */
  x > (tf?(!tf):64); /* { dg-bogus "signed and unsigned" "case 35" } */
  y > (tf?(!tf):64); /* { dg-bogus "signed and unsigned" "case 36" } */

}
