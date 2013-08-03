/* Test that IPA-CP is able to produce a pass-through jump function for the
   call of g1 and g2 even though a is addressable.  Also test that h is not
   cloned.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fdump-ipa-cp -fno-early-inlining"  } */
/* { dg-add-options bind_pic_locally } */

extern void use_stuff (int);
extern void use_pointer (int *);

static int
h (int a, int b)
{
  int i;

  for (i = 8; i <= b; i++)
    use_stuff (a+8);
}

static int
g1 (int a, int b)
{
  int i;

  for (i = 0; i <= b; i++)
    use_pointer (&a);
  h (a, b);
}

static int
g2 (int a, int b)
{
  int i;

  for (i = 4; i <= b; i += 2)
    use_stuff (a);
}


static void
f (int a, int z)
{
  if (z > 1)
    g1 (a, z);
  else
    g2 (a + 4, z);
  use_pointer (&a);
}

int
main (int argc, char *argv[])
{
  int i;
  for (i = 0; i < 100; i++)
    f (7, argc);
  return 0;
}


/* { dg-final { scan-ipa-dump "Creating a specialized node of g1.*for all known contexts" "cp" } } */
/* { dg-final { scan-ipa-dump "Creating a specialized node of g2.*for all known contexts" "cp" } } */
/* { dg-final { scan-ipa-dump-not "Creating a specialized node of h.*for all known contexts" "cp" } } */
/* { dg-final { scan-ipa-dump-times "replacing param .0 a with const 7" 2 "cp"  } } */
/* { dg-final { scan-ipa-dump "replacing param .0 a with const 11" "cp"  } } */
/* { dg-final { cleanup-ipa-dump "cp" } } */


