/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

__attribute__ ((altivec(vector__))) long long
sdiv (__attribute__ ((altivec(vector__))) long long a,
      __attribute__ ((altivec(vector__))) long long b)
{
  return __builtin_vsx_div_2di (a, b);
}
__attribute__ ((altivec(vector__))) unsigned long long
udiv (__attribute__ ((altivec(vector__))) unsigned long long a,
      __attribute__ ((altivec(vector__))) unsigned long long b)
{
  return __builtin_vsx_udiv_2di (a, b);
}
__attribute__ ((altivec(vector__))) long long
smul (__attribute__ ((altivec(vector__))) long long a,
      __attribute__ ((altivec(vector__))) long long b)
{
  return __builtin_vsx_mul_2di (a, b);
}
