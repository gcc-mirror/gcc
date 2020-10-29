/* { dg-do compile } */
/* { dg-options "-O2" } */

void
q8 (__int128 *uv, unsigned short int nf)
{
  __int128 i4;

  i4 = -nf;
  if (i4 << 1 != 0)
    *uv += nf;
}
