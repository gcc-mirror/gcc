/* PR target/109178 */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Verify we do not ICE on the following.  */

typedef __attribute__ ((altivec (vector__))) signed __int128 v1ti_t;

void
foo (signed int *dst, v1ti_t src)
{
  __builtin_vec_xst_trunc(src, 0, dst);
}
