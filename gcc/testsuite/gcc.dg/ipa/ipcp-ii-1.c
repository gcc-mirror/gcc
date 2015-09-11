/* Verify that simple indirect calls are inlined even without early
   inlining..  */
/* { dg-do compile } */
/* { dg-options "-O3 -c -fdump-ipa-inline -fno-early-inlining"  } */

extern void non_existent(int);
extern void non_existent(int);

static void hooray ()
{
  non_existent (1);
}

static void  __attribute__ ((noinline)) hiphip (void (*f)())
{
  f ();
}

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

int main (int argc, int *argv[])
{
  int i;

  for (i = 0; i < get_input (); i++)
    hiphip (hooray);
  return 0;
}

/* { dg-final { scan-ipa-dump "hooray\[^\\n\]*inline copy in hiphip.constprop"  "inline"  } } */
