/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef int UDItype __attribute__ ((mode (DI)));
typedef __attribute__ ((aligned)) struct
{
  UDItype w[2];
} UINT128;

UINT128
__bid128_copySign (UINT128 x)
{
  x.w[1] = x.w[1] & 8000000000000000ULL;
  return x;
}
