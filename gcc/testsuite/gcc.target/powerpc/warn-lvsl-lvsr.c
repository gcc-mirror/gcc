/* Test for deprecation messages on use of lvsl and lvsr for little endian.  */

/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-options "-O0 -Wdeprecated" } */

#include <altivec.h>

float f[20];

void foo ()
{
  vector unsigned char a = vec_lvsl (4, f); /* { dg-warning "'vec_lvsl' is deprecated for little endian; use assignment for unaligned loads and stores" } */
  vector unsigned char b = vec_lvsr (8, f); /* { dg-warning "'vec_lvsr' is deprecated for little endian; use assignment for unaligned loads and stores" } */
}
