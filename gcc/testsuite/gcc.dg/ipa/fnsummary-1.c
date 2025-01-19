/* { dg-options "-O2 -fdump-ipa-fnsummary-details"  } */
int
test(int a, int b)
{
	if (a / b > 10)
		__builtin_unreachable ();
}
/* { dg-final { scan-ipa-dump "ends with conditional guarding __builtin_unreachable" "fnsummary"  } } */
/* { dg-final { scan-ipa-dump-times "skipping unnecesary stmt" 2 "fnsummary"  } } */
