/* We do not have a direct conversion instruction from 128 bit DFP to
   32 bit DFP so this needs to be done in two steps.  The first needs
   to be done with the "prepare for shorter precision rounding mode"
   in order to produce a correct result.  Otherwise the 8th digit of
   the number will change from 4 to 5 in the first rounding step which
   then will turn the last digit of the 32 bit DFP number (the 3) into
   a 4.  Although with direct rounding it would stay a 3.  */

/* { dg-do run } */
/* { dg-options "-O3 -march=z10 -mzarch" } */

_Decimal32 __attribute__((noinline))
foo (_Decimal128 a)
{
  return (_Decimal32)a;
}

int
main ()
{
    if (foo (1.23456349999999999DL) != 1.234563DF)
    __builtin_abort ();
}
