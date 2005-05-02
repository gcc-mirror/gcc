/* Spurious uninitialized variable warning, inspired by libgcc2.c.  */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

/* Not all platforms support TImode integers.  */
#if defined(__LP64__) || defined(__sparc__)
typedef int TItype __attribute__ ((mode (TI)));  /* { dg-error "no data type for mode" "TI" { target sparc-sun-solaris2.[0-6] sparc-sun-solaris2.[0-6].* } } */
#else
typedef long TItype;
#endif


TItype
__subvdi3 (TItype a, TItype b)
{
  TItype w;
  
  w = a - b;
  
  return w;
}
