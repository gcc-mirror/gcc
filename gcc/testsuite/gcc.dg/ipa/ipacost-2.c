/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fdump-ipa-cp -fno-early-inlining -fno-ipa-sra -fdump-tree-optimized -fno-ipa-icf"  } */
/* { dg-add-options bind_pic_locally } */

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
  int i;
  i_can_not_be_propagated_fully (a);
  for (i=0;i<50;i++)
  {
    t(a[i] + 1);
    t(a[i+1] + 1);
    t(a[i+2] + 1);
    t(a[i+3] + 1);
  }
  i_can_not_be_propagated_fully (a);
  for (i=0;i<50;i++)
  {
    t(a[i] + 2);
    t(a[i+1] + 2);
    t(a[i+2] + 2);
    t(a[i+3] + 2);
  }
  i_can_not_be_propagated_fully (a);
}
int
main()
{
  int i;
  i_can_be_propagated_fully2 (array);
  i_can_be_propagated_fully2 (array);

  for (i = 0; i < 7; i++)
    i_can_not_be_propagated_fully2 (array);
  i_can_not_be_propagated_fully2 (array);
}

/* { dg-final { scan-ipa-dump-times "Creating a specialized node of i_can_be_propagated_fully2" 1 "cp"  } } */
/* { dg-final { scan-ipa-dump-times "Creating a specialized node of i_can_be_propagated_fully\[./\]" 1 "cp"  } } */
/* { dg-final { scan-ipa-dump-not "Creating a specialized node of i_can_not_be_propagated_fully2" "cp"  } } */
/* { dg-final { scan-ipa-dump-not "Creating a specialized node of i_can_not_be_propagated_fully/" "cp"  } } */
/* { dg-final { scan-tree-dump-not "i_can_be_propagated_fully \\(" "optimized"  } } */
/* { dg-final { scan-tree-dump-not "i_can_be_propagated_fully2 \\(" "optimized"  } } */
