/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-maltivec -fpreprocessed" } */
/* { dg-require-effective-target powerpc_altivec } */

/* Program to test AltiVec with -fpreprocessed.  */
int foo(__attribute__((altivec(vector__))) float x,
        __attribute__((altivec(vector__))) float y)
{
  if (__builtin_vec_vcmpeq_p (2, (x), (y)))
    return 3245;
  else
    return 12;
}
