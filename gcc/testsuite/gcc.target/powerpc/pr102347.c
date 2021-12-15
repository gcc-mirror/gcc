/* { dg-do link } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-require-effective-target powerpc_p9modulo_ok } */
/* { dg-require-effective-target lto } */
/* { dg-options "-flto -mdejagnu-cpu=power8" } */

/* Verify there are no error messages in LTO mode.  */

#pragma GCC target "cpu=power9"
int main ()
{
  int res;
#ifdef __LP64__
  res = (int) __builtin_darn ();
#else
  res = __builtin_darn_32 ();
#endif
  return res;
}
