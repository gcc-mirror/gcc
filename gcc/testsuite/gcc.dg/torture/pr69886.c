/* PR rtl-optimization/69886.  */
/* { dg-do compile } */
/* { dg-options "--param=gcse-unrestricted-cost=0 -w -Wno-psabi" } */
/* { dg-additional-options "-mavx" { target { i?86-*-* x86_64-*-* } } } */

typedef unsigned v32su __attribute__ ((vector_size (32)));

unsigned
foo (v32su v32su_0, v32su v32su_1, v32su v32su_2, v32su v32su_3, v32su v32su_4)
{
  v32su_3 += v32su_2 *= v32su_2[3];
  if (v32su_4[3])
    v32su_2 &= (v32su){ v32su_1[3], 0xbb72, 64 };
  return v32su_0[2] + v32su_2[4] + v32su_3[1];
}
