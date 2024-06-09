/* { dg-do compile } */
/* { dg-additional-options "-O" } */
/* { dg-additional-options "-march=silvermont" { target { x86_64-*-* i?86-*-* } } } */

long *BN_uadd_ap;

void
BN_uadd (int dif, long t1)
{
  long *rp;
  while (dif)
    {
      dif--;
      t1 = *BN_uadd_ap;
      *rp++ = t1;
      if (t1)
        break;
    }
}
