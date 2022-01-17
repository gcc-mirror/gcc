/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */
/* { dg-final { scan-assembler-not {\mmr\M} } } */

vector __int128 add (long long a)
{
  vector __int128 b;
  b = (vector __int128) {a};
  return b;
}
