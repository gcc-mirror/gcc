/* Test for correct handling of literal arguments. */
/* Author: Ziemowit Laski  <zlaski@apple.com>  */
/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-xfail-if "" { "powerpc-*-eabispe*" "powerpc-ibm-aix*" } { "*" } { "" } } */
/* { dg-options "-maltivec" } */

#include <altivec.h>

void foo(void) {
  const unsigned char *buf;
  vector pixel vp = { 3, 4, 5, 6 };
  vector bool int vbi = { 1, 0, 1, 0 };
  vector bool short vbs = { 1, 0, 1, 0, 1, 0, 1, 0 };
  vector bool char vbc = { 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0 };
  vector signed char vsc;
  int a = 3;
  
  vec_dst(buf, a, 1);
  vec_dstst(buf, a, 2);
  vec_dststt(buf, a, 3);
  vec_dststt(buf, a, 2);

  vp = vec_sld(vp, vp, 5);
  vbc = vec_splat(vbc, 7);
  vbs = vec_splat(vbs, 12);
  vp = vec_splat(vp, 17);
  vbi = vec_splat(vbi, 31);  
}
