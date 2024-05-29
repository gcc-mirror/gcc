/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target int128 } */
/* { dg-final { scan-assembler-not {\mmr\M} } } */

vector __int128 add (long long a)
{
  vector __int128 b;
  b = (vector __int128) {a};
  return b;
}
