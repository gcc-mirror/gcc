/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O -fdump-ipa-tmipa" } */

int val, george;

extern void func();

int set_remove(void)
{
  int result = 8;
  __transaction_atomic  {
    result = george;
    if (val)
      goto out;
  }
 out:
  func();
  return result;
}


/* { dg-final { scan-ipa-dump-not "getTMCloneOrIrrevocable" "tmipa" } } */
/* { dg-final { cleanup-ipa-dump "tmipa" } } */
