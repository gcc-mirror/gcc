/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-mdejagnu-cpu=power6 -maltivec" } */

/* This used to ICE.  */

typedef long long v2di __attribute__ ((vector_size (16)));

v2di
foo_v2di_l (v2di x)
{
  return __builtin_shuffle ((v2di){0, 0}, x, (v2di){3, 0});
}

