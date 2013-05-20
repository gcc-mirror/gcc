/* Verify that indirect inlining can also remove references of the functions it
   discovers calls for.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fno-early-inlining -fno-ipa-cp -fdump-ipa-inline -fdump-tree-optimized"  } */

int global;

void __attribute__ ((noinline, noclone, used))
stuff (int i)
{
  global = i;
}

static void hooray ()
{
  stuff (1);
}

static void hiphip (void (*f)())
{
  stuff (2);
  f ();
}

int main (void)
{
  hiphip (hooray);
  return 0;
}

/* { dg-final { scan-ipa-dump "ipa-prop: Removed a reference"  "inline"  } } */
/* { dg-final { scan-tree-dump-not "hooray"  "optimized"  } } */
/* { dg-final { cleanup-ipa-dump "inline" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
