/* { dg-do run } */
/* { dg-options "-fsanitize=shift -w" } */

typedef const unsigned long long int CULLI;
typedef volatile int VI;
struct s { signed long int a; };

int
main (void)
{
  int a = 1;
  struct s s = { .a = 400 };
  CULLI culli = 42;
  VI vi = 370;
  volatile int shiftcount = 153;

  a <<= 152;
  1 << shiftcount;
  1 << 154;
  culli << 524;
  1 << vi++;
  (long) 1 << (s.a + 2);

  return 0;
}
/* { dg-output "shift exponent 152 is too large for \[^\n\r]*-bit type 'int'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent 153 is too large for \[^\n\r]*-bit type 'int'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent 154 is too large for \[^\n\r]*-bit type 'int'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent 524 is too large for \[^\n\r]*-bit type 'long long unsigned int'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent 370 is too large for \[^\n\r]*-bit type 'int'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent 402 is too large for \[^\n\r]*-bit type 'long int'(\n|\r\n|\r)" } */
