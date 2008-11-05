/* PR middle-end/37858 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-early_local_cleanups -dv" } */

int
main (void)
{
  return 0;
}

/* { dg-final { cleanup-ipa-dump "early_local_cleanups" } } */
