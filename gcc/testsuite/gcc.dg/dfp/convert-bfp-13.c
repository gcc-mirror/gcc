/* Test for bug where fold changed binary operation to decimal
   depending on typedefs.  */
/* { dg-options "-std=gnu99" } */

extern void abort (void);
extern void exit (int);

volatile double d = 1.2345675;

typedef const volatile _Decimal32 d32;

int
main (void)
{
  _Decimal32 a = (d * d);
  d32 b = (d * d);
  if (a != b)
    abort ();
  exit (0);
}
