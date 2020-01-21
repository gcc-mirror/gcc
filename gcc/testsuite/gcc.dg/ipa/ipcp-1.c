/* Test that IPA-CP is able to figure out that both parameters a are constant 7
   even though f and h recursively call each other and specialize them
   accordingly.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fdump-ipa-cp -fno-early-inlining"  } */
/* { dg-add-options bind_pic_locally } */

extern void use_stuff (int);

static
int g (int b, int c)
{
  int i;

  for (i = 0; i < b; i++)
    use_stuff (c);
}

static void f (int a, int x, int z);

static void h (int z, int a)
{
  use_stuff (z);
  f (a, 9, 10);

}

static void
f (int a, int x, int z)
{
  if (z > 1)
    g (a, x);
  else
    h (5, a);
}

int
main (int argc, char *argv[])
{
  int i;
  for (i = 0; i < 100; i++)
    f (7, 8, argc);
  return 0;
}


/* { dg-final { scan-ipa-dump "Creating a specialized node of f" "cp" } } */
/* { dg-final { scan-ipa-dump "replacing param .0 a with const 7" "cp"  } } */


