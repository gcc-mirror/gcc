/* Verify that analysis of function parameters works as expected.  */
/* { dg-do compile } */
/* { dg-options "-O3 -c -fdump-ipa-inline"  } */
struct bah {int a,b,c,d,e;};
static struct bah bah3={2,3,4,5,6};
const static struct bah bah4={2,3,4,5,6};
void test (int, struct bah *, struct bah, struct bah, int, struct bah, struct bah, struct bah);
void foo (int invariant, struct bah invariant2)
{
  int i;
  struct bah bah2={1,2,3,4,5};
  struct bah bah5={1,2,3,4,5};
  for (i = 0; i<10; i++)
    {
      bah5.a=i;
      test (i, &bah2, bah2, bah3, invariant, invariant2, bah4, bah5);
    }
}
/* op0 change on every invocation.  */
/* op1 is function invariant.  */
/* { dg-final { scan-ipa-dump-not "op0 is compile time invariant"  "inline"  } } */
/* { dg-final { scan-ipa-dump-not "op0 change"  "inline"  } } */
/* { dg-final { scan-ipa-dump "op1 is compile time invariant"  "inline"  } } */
/* op2 is invariant within loop (we make assumption that function call does not afect it.). */
/* { dg-final { scan-ipa-dump "op2 change 10.000000. of time"  "inline"  } } */
/* op3 is invariant within loop (we make assumption that function call does not afect it.). */
/* { dg-final { scan-ipa-dump "op3 change 10.000000. of time"  "inline"  } } */
/* op4 is invariant within loop.  */
/* { dg-final { scan-ipa-dump "op4 change 10.000000. of time"  "inline"  } } */
/* op5 is invariant within loop.  */
/* { dg-final { scan-ipa-dump "op5 change 10.000000. of time"  "inline"  } } */
/* op6 is compile time invariant.  */
/* { dg-final { scan-ipa-dump "op6 is compile time invariant"  "inline"  } } */
/* op7 change.  */
/* { dg-final { scan-ipa-dump-not "op7 is compile time invariant"  "inline"  } } */
/* { dg-final { scan-ipa-dump-not "op7 change"  "inline"  } } */
