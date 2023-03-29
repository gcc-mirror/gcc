/* { dg-options "-O2" } */

/* This used to ICE with the SYSV ABI (PR96072).  */

void
he (int jn)
{
  {
    int bh[jn];
    if (jn != 0)
      goto wa;
  }
wa:;
}
