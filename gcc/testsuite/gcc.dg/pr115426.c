/* { dg-do compile } */
/* { dg-options "-std=gnu11" } */

_Complex int fcs (_Complex int r)
{
  __asm__("" : "=rm" (__imag__ r));
  return r;
}

_Complex int fcs2 (_Complex int r)
{
  __asm__("" : "=m" (__imag__ r));
  return r;
}
