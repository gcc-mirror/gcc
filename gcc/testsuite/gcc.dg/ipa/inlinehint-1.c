/* { dg-options "-O3 -c -fdump-ipa-inline-details -fno-early-inlining -fno-ipa-cp"  } */
test (int a)
{
   int i;
   for (i=0; i<a; i++)
{
     test2(a);
     test2(a);
}
}
m()
{
  test (10);
}
/* { dg-final { scan-ipa-dump "loop_iterations"  "inline"  } } */
/* { dg-final { cleanup-ipa-dump "inline" } } */
