/* { dg-options "-O2 -fno-early-inlining -fdump-ipa-profile-optimized -fdump-ipa-afdo-optimized" } */
volatile int one;
static int
add1 (int val)
{
  return val += one;
}

static int
sub1 (int val)
{
  return val -= one;
}

static int
do_op (int val, int (*fnptr) (int))
{
  return fnptr (val);
}

int
main (void)
{
  int i, val = 0;
  for (i = 0; i < 10000000; i++)
    {
      val = do_op (val, add1);
      val = do_op (val, sub1);
    }
  return val;
}
/* { dg-final-use-not-autofdo { scan-ipa-dump "Indirect call -> direct call.* add1 .will resolve by ipa-profile" "profile"} } */
/* { dg-final-use-not-autofdo { scan-ipa-dump "Indirect call -> direct call.* sub1 .will resolve by ipa-profile" "profile"} } */
/* { dg-final-use-autofdo { scan-ipa-dump "Inlining add1/1 into main/4." "afdo"} } */
/* { dg-final-use-autofdo { scan-ipa-dump "Inlining sub1/2 into main/4." "afdo"} } */
