/* { dg-do compile } */
/* { dg-additional-options "-O3 -fno-tree-pre" } */
/* { dg-additional-options "-mavx512bw" { target x86_64-*-* i?86-*-* } } */

void
ll (char *un, char *rr, int te, int fp, int nb)
{
  const int xe = nb & 1;

  while (fp-- != 0)
    {
      if ((rr[0] & xe) == 0)
        un[0] = 0;

      un += te;
      rr += te;
    }
}
