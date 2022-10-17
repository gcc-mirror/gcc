/* { dg-do compile } */
/* { dg-options "-Os -c -fdump-ipa-inline-details -fno-early-inlining -fno-partial-inlining"  } */
void link_error ();
int
test(int a)
{
  if (a>10)
	{
		link_error ();
		link_error ();
		link_error ();
		link_error ();
		link_error ();
		link_error ();
		link_error ();
		link_error ();
		link_error ();
		link_error ();
		link_error ();
		link_error ();
		link_error ();
		link_error ();
		link_error ();
		link_error ();
	}
}
int test2()
{
	for (int i=0;i<10;i++)
		test(i);
}
/* { dg-final { scan-ipa-dump "Known to be false: not inlined, op0 > 10"  "inline"  } } */
/* { dg-final { scan-ipa-dump "Inlined test"  "inline"  } } */
