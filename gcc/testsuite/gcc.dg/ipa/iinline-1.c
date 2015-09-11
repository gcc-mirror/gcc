/* Verify that simple indirect calls are inlined even without early
   inlining..  */
/* { dg-do compile } */
/* { dg-options "-O3 -c -fdump-ipa-inline-details -fno-early-inlining -fno-ipa-cp"  } */

extern void non_existent(int);

static void hooray ()
{
  non_existent (1);
}

static void hiphip (void (*f)())
{
  non_existent (2);
  f ();
}

int test (void)
{
  hiphip (hooray);
  return 0;
}

/* { dg-final { scan-ipa-dump "indirect_call"  "inline"  } } */
/* { dg-final { scan-ipa-dump "hooray\[^\\n\]*inline copy in test"  "inline"  } } */
