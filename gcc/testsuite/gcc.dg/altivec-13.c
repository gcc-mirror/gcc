/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-xfail-if "" { "powerpc-ibm-aix*" } { "-maltivec" } { "" } } */
/* { dg-options "-maltivec" } */
/* Author: Ziemowit Laski  <zlaski@apple.com>  */

/* This test case exercises intrinsic/argument combinations that,
   while not in the Motorola AltiVec PIM, have nevertheless crept
   into the AltiVec vernacular over the years.  */

#include <altivec.h>

void foo (void) 
{
  vector bool int boolVec1 = (vector bool int) vec_splat_u32(3);
  vector bool short boolVec2 = (vector bool short) vec_splat_u16(3);
  vector bool char boolVec3 = (vector bool char) vec_splat_u8(3);

  boolVec1 = vec_sld( boolVec1, boolVec1, 4 );
  boolVec2 = vec_sld( boolVec2, boolVec2, 2 );
  boolVec3 = vec_sld( boolVec3, boolVec3, 1 );
}
