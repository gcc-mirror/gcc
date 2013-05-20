/* Verify that indirect inlining machinery can remove references to functions
   passed as parameters that are never used.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fno-early-inlining -fno-ipa-sra -fno-ipa-cp -fdump-ipa-inline -fdump-tree-optimized"  } */

extern int __attribute__ ((noinline, noclone, used))
stuff (int i)
{
  return 0;
}

static void hooray ()
{
  stuff (1);
}

static int hiphip (void (*f)())
{
  return stuff (2);
}

int main (void)
{
  return hiphip (hooray);
}

/* { dg-final { scan-ipa-dump "ipa-prop: Removed a reference"  "inline"  } } */
/* { dg-final { scan-tree-dump-not "hooray"  "optimized"  } } */
/* { dg-final { cleanup-ipa-dump "inline" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
