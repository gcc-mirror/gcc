/* Spurious uninitialized variable warning, inspired by libgcc2.c.  */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

typedef int TItype __attribute__ ((mode (TI)));

TItype
__subvdi3 (TItype a, TItype b)
{
  TItype w;
  
  w = a - b;
  
  return w;
}
