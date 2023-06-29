/* { dg-do compile } */
/* { dg-options "-Os -fdump-ipa-inline-details" } */
void bad (void);
void
test(int a)
{
	if (__builtin_expect (a>3, 0))
	{
		bad ();
		bad ();
		bad ();
		bad ();
		bad ();
		bad ();
		bad ();
		bad ();
	}
}
void
foo (int a)
{
	if (a>0)
		__builtin_unreachable ();
	test (a);
}
/* { dg-final { scan-ipa-dump "Inlined 2 calls" "inline"  } } */
/* { dg-final { scan-ipa-dump "Inlining test" "inline"  } } */
