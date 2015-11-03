/* { dg-do run } */
/* { dg-options "-O -mfp16-format=ieee" } */

/* Conversion of infinity to __fp16 and back again should preserve the
   value.  */
volatile float f = __builtin_inf ();

int main ()
{
  __fp16 h = f;
  if (h != __builtin_inf ())
    __builtin_abort ();
  return 0;
}
