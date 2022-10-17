/* PR target/101849 */
/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Verify we do not ICE on the tests below.  */

void
foo (__vector_pair *dst, double *x, long offset)
{
  dst[0] = __builtin_vsx_lxvp (0, (__vector_pair *)(void *)x);
  dst[1] = __builtin_vsx_lxvp (32, (__vector_pair *)(void *)x);
  dst[2] = __builtin_vsx_lxvp (offset, (__vector_pair *)(void *)x);
}

void
bar (__vector_pair *src, double *x, long offset)
{
  __builtin_vsx_stxvp (src[0], 0, (__vector_pair *)(void *)x);
  __builtin_vsx_stxvp (src[1], 32, (__vector_pair *)(void *)x);
  __builtin_vsx_stxvp (src[2], offset, (__vector_pair *)(void *)x);
}
