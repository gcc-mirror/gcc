/* We do not have hardware instructions which do a direct conversion
   between the 32 and 128 bit DFP types.  But we can easily do it in
   two steps.  Older libdfp implementations require this not to call
   into a lib in order to prevent an endless loop.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=z10 -mzarch" } */

_Decimal32 
foo (_Decimal128 a)
{
  return (_Decimal32)a;
}

_Decimal128
bar (_Decimal32 a)
{
  return (_Decimal128)a;
}

/* Make sure no library call is emitted.  */
/* { dg-final { scan-assembler-not "brasl" } } */
