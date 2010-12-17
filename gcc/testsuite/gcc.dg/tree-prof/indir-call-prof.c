/* { dg-options "-O2 -fdump-tree-optimized -fdump-ipa-tree_profile_ipa" } */

static int a1 (void)
{
    return 10;
}

static int a2 (void)
{
    return 0;
}

typedef int (*tp) (void);

static tp aa [] = {a2, a1, a1, a1, a1};

void setp (int (**pp) (void), int i)
{
  if (!i)
    *pp = aa [i];
  else
    *pp = aa [(i & 2) + 1];
}

int
main (void)
{
  int (*p) (void);
  int  i;

  for (i = 0; i < 10; i ++)
    {
	setp (&p, i);
	p ();
    }
  
  return 0;
}

/* { dg-final-use { scan-ipa-dump "Indirect call -> direct call.* a1 transformation on insn" "tree_profile_ipa"} } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */
/* { dg-final-use { cleanup-tree-dump "optimized" } } */
/* { dg-final-use { cleanup-ipa-dump "tree_profile_ipa" } } */
