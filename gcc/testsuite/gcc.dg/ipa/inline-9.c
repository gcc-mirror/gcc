/* { dg-options "-Os -fdump-ipa-inline-details"  } */
int foo (void);
int test(int a)
{
 if (a>100)
   {
     foo();
     foo();
     foo();
     foo();
     foo();
     foo();
     foo();
     foo();
   }
}
int
main()
{
  for (int i=0;i<100;i++)
    test(i);
}
/* { dg-final { scan-ipa-dump "Inlined 2 calls" "inline" } } */
