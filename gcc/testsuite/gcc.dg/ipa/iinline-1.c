/* Verify that simple indirect calls are inlined even without early
   inlining..  */
/* { dg-do compile } */
/* { dg-options "-O3 -c -fdump-ipa-inline -fno-early-inlining"  } */

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

int main (int argc, int *argv[])
{
  hiphip (hooray);
  return 0;
}

/* { dg-final { scan-ipa-dump "hooray\[^\\n\]*inline copy in main"  "inline"  } } */
/* { dg-final { cleanup-ipa-dump "inline" } } */
