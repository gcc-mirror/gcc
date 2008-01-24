/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -msse2" { target { i?86-*-* x86_64-*-* } } } */

#undef __vector
#define __vector __attribute__((vector_size(16) ))

typedef __vector signed char qword;
typedef __vector unsigned int VU32;

extern short g[192 + 16];

void f (qword);

void f1 (unsigned ctr)
{
  VU32 pin;
  pin = (VU32){(unsigned int) &g[16]};
  do
    {
      f ((qword) pin);
      ctr--;
    }
  while (ctr);
}
