/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */
/* { dg-require-effective-target float16_runtime } */
/* { dg-require-effective-target dfprt } */
/* { dg-options "-save-temps" } */
/* { dg-add-options float16 } */

/* Test conversions from DFP to smaller types.  */

_Decimal32 var32;
_Decimal64 var64;
_Decimal128 var128;
_Float16 var16;

void __attribute__ ((__noinline__)) foo32 (_Decimal32 param32)
{
  var16 = param32;
}

void __attribute__ ((__noinline__)) foo64 (_Decimal64 param64)
{
  var16 = param64;
  var32 = param64;
}

void __attribute__ ((__noinline__)) foo128 (_Decimal128 param128)
{
  var16 = param128;
  var32 = param128;
  var64 = param128;
}

int main ()
{
  foo32 (var32);
  foo64 (var64);
  foo128 (var128);
  return 0;
}

/* { dg-final { scan-assembler-times {\t__bid_truncsdhf} 2 { target { dfp_bid } } } } */
/* { dg-final { scan-assembler-times {\t__bid_truncddhf} 2 { target { dfp_bid } } } } */
/* { dg-final { scan-assembler-times {\t__bid_truncddsd2} 2 { target { dfp_bid } } } } */
/* { dg-final { scan-assembler-times {\t__bid_trunctdhf} 2 { target { dfp_bid } } } } */
/* { dg-final { scan-assembler-times {\t__bid_trunctdsd2} 2 { target { dfp_bid } } } } */
/* { dg-final { scan-assembler-times {\t__bid_trunctddd2} 2 { target { dfp_bid } } } } */
