/* { dg-do run } */
/* { dg-require-effective-target float16_runtime } */
/* { dg-require-effective-target dfprt } */
/* { dg-options "-save-temps" } */
/* { dg-add-options float16 } */

/* Test conversions to/from DFP values.  */

extern void abort ();

_Decimal32 var32 = 1.2df;

int __attribute__ ((__noinline__)) foo32 (_Decimal32 param32, _Decimal64 param64, _Decimal128 param128, _Float16 param16)
{
  return (param32 == var32)
    + (param64 == var32)
    + (param128 == var32)
    /* Small enough relative difference?  */
    + ((((_Decimal32)param16 - var32) / var32) < 0.002df);
}

_Decimal64 var64 = 1.2dd;

int __attribute__ ((__noinline__)) foo64 (_Decimal32 param32, _Decimal64 param64, _Decimal128 param128, _Float16 param16)
{
  return (param32 == var64)
    + (param64 == var64)
    + (param128 == var64)
    /* Small enough relative difference?  */
    + ((((_Decimal64)param16 - var64) / var64) < 0.002dd);
}

_Decimal128 var128 = 1.2dl;

int __attribute__ ((__noinline__)) foo128 (_Decimal32 param32, _Decimal64 param64, _Decimal128 param128, _Float16 param16)
{
  return (param32 == var128)
    + (param64 == var128)
    + (param128 == var128)
    /* Small enough relative difference?  */
    + ((((_Decimal128)param16 - var128) / var128) < 0.002dl);
}

int main()
{
  if (foo32 (1.2df, 1.2dd, 1.2dl, (_Float16)1.2) != 4)
    abort ();

  if (foo64 (1.2df, 1.2dd, 1.2dl, (_Float16)1.2) != 4)
    abort ();

  if (foo128 (1.2df, 1.2dd, 1.2dl, (_Float16)1.2) != 4)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times {\t__bid_extendsddd2} 3 { target { dfp_bid } } } } */
/* { dg-final { scan-assembler-times {\t__bid_extendsdtd2} 3 { target { dfp_bid } } } } */
/* { dg-final { scan-assembler-times {\t__bid_extendddtd2} 3 { target { dfp_bid } } } } */
/* { dg-final { scan-assembler-times {\t__bid_extendhfsd} 2 { target { dfp_bid } } } } */
/* { dg-final { scan-assembler-times {\t__bid_extendhfdd} 2 { target { dfp_bid } } } } */
/* { dg-final { scan-assembler-times {\t__bid_extendhftd} 2 { target { dfp_bid } } } } */
