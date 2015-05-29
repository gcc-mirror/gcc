/* Verify that simple indirect calls are inlined even without early
   inlining..  */
/* { dg-do compile } */
/* { dg-options "-O3 -c -fdump-ipa-inline -fno-early-inlining -fno-ipa-cp"  } */

extern void non_existent(int);

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

static void hooray ()
{
  non_existent (1);
}

static void hip2 (void (*g)())
{
  non_existent (2);
  g ();
}

static void hip1 (void (*f)(void (*)()), void (*g)())
{
  non_existent (2);
  f (g);
}

int main (int argc, int *argv[])
{
  int i;

  for (i = 0; i < get_input (); i++)
    hip1 (hip2, hooray);
  return 0;
}

/* { dg-final { scan-ipa-dump "hooray\[^\\n\]*inline copy in main" "inline"  } } */
/* { dg-final { scan-ipa-dump "hip2\[^\\n\]*inline copy in main" "inline"  } } */
