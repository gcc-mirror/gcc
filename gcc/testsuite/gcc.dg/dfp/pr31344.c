/* { dg-do compile } */
/* { dg-options "-O -mtune=i386" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-options "-O" } */

typedef struct
{
  unsigned char bits;
} decNumber;

typedef struct
{
  unsigned char bytes[1];
} decimal32;

extern decNumber *__decimal32ToNumber (const decimal32 *, decNumber *);
extern void __host_to_ieee_32 (_Decimal32, decimal32 *);

void
foo (_Decimal32 arg)
{
  decNumber dn;
  decimal32 d32;
  __host_to_ieee_32 (arg, &d32);
  __decimal32ToNumber (&d32, &dn);
}
