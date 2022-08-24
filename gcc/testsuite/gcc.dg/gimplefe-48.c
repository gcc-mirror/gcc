/* { dg-do compile } */
/* { dg-options "-O -fgimple -Wno-psabi -w" } */
/* { dg-additional-options "-mavx2" { target x86_64-*-* i?86-*-* } } */

typedef int v8si __attribute__((vector_size(8*sizeof(int))));
typedef v8si v8sib __attribute__((vector_mask));

v8si res;

void __GIMPLE (ssa) foo (v8si v1, v8si v2, v8si v3, v8si v4)
{
  v8sib tem;
  v8si resr;

__BB(2):
  tem_3 = v1_1(D) <= v2_2(D);
  resr_4 = tem_3 ? v3_5(D) : v4_6(D);
  res = resr_4;
  return;
}
