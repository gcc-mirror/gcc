/* PR middle-end/128460 */
/* { dg-do run } */
/* { dg-options "-O1" } */

extern void abort (void);

float f = 16777215;
_Decimal32 d32 = 1.000001DF;
_Decimal32 tiny = 1E-50DF;

/* Binary type to the same type through decimal.  */
__attribute__ ((noipa)) float
bin_dec_bin (float x)
{
  return (float) (_Decimal32) x;
}

/* Decimal type to the same type through binary.  */
__attribute__ ((noipa)) _Decimal32
dec_bin_dec (_Decimal32 x)
{
  return (_Decimal32) (float) x;
}

/* Decimal to wider binary through narrower binary.  */
__attribute__ ((noipa)) double
dec_bin_wide (_Decimal32 x)
{
  return (double) (float) x;
}

/* Binary to decimal through a wider decimal type, which is still exact.  */
__attribute__ ((noipa)) _Decimal64
bin_dec_dec (float x)
{
  return (_Decimal64) (_Decimal32) x;
}

int
main (void)
{
  _Decimal32 d = f;
  float r = d;
  if (r != 16777220.0f)
    abort ();
  if (bin_dec_bin (f) != 16777220.0f)
    abort ();
  /* Folded by GENERIC.  */
  if ((float) (_Decimal32) 16777215.0f != 16777220.0f)
    abort ();
  if (dec_bin_dec (tiny) != 0)
    abort ();
  volatile float t = d32;
  double e = t;
  if (dec_bin_wide (d32) != e)
    abort ();
  volatile _Decimal32 u = f;
  _Decimal64 e2 = u;
  if (bin_dec_dec (f) != e2)
    abort ();
  return 0;
}
