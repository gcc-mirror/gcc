/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1 -Wno-psabi -w" } */
/* { dg-additional-options "-msse2" { target i?86-*-* x86_64-*-* } } */
/* { dg-additional-options "-maltivec" { target powerpc_altivec_ok } } */

typedef int v4si __attribute__((vector_size (4 * sizeof (int))));

v4si test1 (v4si v, int i)
{
  ((int *)&v)[0] = i;
  return v;
}

v4si test2 (v4si v, int i)
{
  int *p = (int *)&v;
  *p = i;
  return v;
}

v4si test3 (v4si v, int i)
{
  ((int *)&v)[3] = i;
  return v;
}

v4si test4 (v4si v, int i)
{
  int *p = (int *)&v;
  p += 3;
  *p = i;
  return v;
}

/* { dg-final { scan-tree-dump-times "Now a gimple register: v" 4 "ccp1" { target { { i?86-*-* x86_64-*-* aarch64*-*-* spu*-*-* } || { powerpc_altivec_ok } } } } } */
