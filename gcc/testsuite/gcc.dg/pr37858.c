/* PR middle-end/37858 */
/* ??? With -dv removed, this test is a bit silly.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-early_local_cleanups" } */

int
main (void)
{
  return 0;
}

/* { dg-final { cleanup-ipa-dump "early_local_cleanups" } } */
