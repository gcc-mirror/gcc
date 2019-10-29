#include <arm_sve.h>

void
test (enum svpattern pat, int i)
{
  svcntb_pat (pat); /* { dg-error {argument 1 of 'svcntb_pat' must be an integer constant expression} } */
  svcntb_pat (i); /* { dg-error {argument 1 of 'svcntb_pat' must be an integer constant expression} } */
  svcntb_pat ((enum svpattern) -1); /* { dg-error {passing 4294967295 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 0);
  svcntb_pat ((enum svpattern) 1);
  svcntb_pat ((enum svpattern) 2);
  svcntb_pat ((enum svpattern) 3);
  svcntb_pat ((enum svpattern) 4);
  svcntb_pat ((enum svpattern) 5);
  svcntb_pat ((enum svpattern) 6);
  svcntb_pat ((enum svpattern) 7);
  svcntb_pat ((enum svpattern) 8);
  svcntb_pat ((enum svpattern) 9);
  svcntb_pat ((enum svpattern) 10);
  svcntb_pat ((enum svpattern) 11);
  svcntb_pat ((enum svpattern) 12);
  svcntb_pat ((enum svpattern) 13);
  svcntb_pat ((enum svpattern) 14); /* { dg-error {passing 14 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 15); /* { dg-error {passing 15 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 16); /* { dg-error {passing 16 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 17); /* { dg-error {passing 17 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 18); /* { dg-error {passing 18 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 19); /* { dg-error {passing 19 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 20); /* { dg-error {passing 20 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 21); /* { dg-error {passing 21 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 22); /* { dg-error {passing 22 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 23); /* { dg-error {passing 23 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 24); /* { dg-error {passing 24 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 25); /* { dg-error {passing 25 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 26); /* { dg-error {passing 26 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 27); /* { dg-error {passing 27 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 28); /* { dg-error {passing 28 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
  svcntb_pat ((enum svpattern) 29);
  svcntb_pat ((enum svpattern) 30);
  svcntb_pat ((enum svpattern) 31);
  svcntb_pat ((enum svpattern) 32); /* { dg-error {passing 32 to argument 1 of 'svcntb_pat', which expects a valid 'enum svpattern' value} } */
}
