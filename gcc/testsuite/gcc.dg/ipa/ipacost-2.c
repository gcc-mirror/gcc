/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fdump-ipa-cp -fno-early-inlining -fdump-tree-optimized"  } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fdump-ipa-cp -fno-early-inlining -fdump-tree-optimized -fpie" { target { ! nonpic } } } */

int array[100];

int t(int);

static int 
i_can_be_propagated_fully (int *a)
{
  int i;
  for (i=0;i<50;i++)
  {
    t(a[i]);
    t(a[i+1]);
    t(a[i+2]);
    t(a[i+3]);
  }
}
static int 
i_can_be_propagated_fully2 (int *a)
{
  i_can_be_propagated_fully (a);
  i_can_be_propagated_fully (a);
  i_can_be_propagated_fully (a);
}
static int 
i_can_not_be_propagated_fully (int *a)
{
  int i;
  for (i=0;i<50;i++)
  {
    t(a[i]);
    t(a[i+1]);
    t(a[i+2]);
    t(a[i+3]);
  }
}
int 
i_can_not_be_propagated_fully2 (int *a)
{
  i_can_not_be_propagated_fully (a);
  i_can_not_be_propagated_fully (a);
  i_can_not_be_propagated_fully (a);
}
main()
{
  i_can_be_propagated_fully2 (array);
  i_can_be_propagated_fully2 (array);
  i_can_not_be_propagated_fully2 (array);
  i_can_not_be_propagated_fully2 (array);
}

/* { dg-final { scan-ipa-dump-times "versioned function i_can_be_propagated_fully2" 1 "cp"  } } */
/* { dg-final { scan-ipa-dump-times "versioned function i_can_be_propagated_fully " 1 "cp"  } } */
/* { dg-final { scan-ipa-dump-times "versioned function i_can_not_be_propagated_fully2" 1 "cp"  } } */
/* { dg-final { scan-ipa-dump-times "versioned function i_can_not_be_propagated_fully " 1 "cp"  } } */
/* { dg-final { scan-tree-dump-not "i_can_be_propagated_fully \\(" "optimized"  } } */
/* { dg-final { scan-tree-dump-not "i_can_be_propagated_fully2 \\(" "optimized"  } } */
/* { dg-final { cleanup-ipa-dump "cp" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
