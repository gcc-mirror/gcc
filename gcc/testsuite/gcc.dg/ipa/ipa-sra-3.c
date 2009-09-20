/* { dg-do compile } */
/* { dg-options "-O2 -fipa-sra -fdump-tree-eipa_sra-details" } */

struct bovid
{
  float red;
  int green;
  void *blue;
};

extern void foo (float, void *, void *, long);

static int
__attribute__((noinline))
ox (struct bovid cow, int z, struct bovid calf, long l)
{
  foo (cow.red, cow.blue, cow.blue, l);
  return 0;
}

void caller (void)
{
  struct bovid cow, calf;

  cow.red = 7.4;
  cow.green = 6;
  cow.blue = &cow;

  calf.red = 8.4;
  calf.green = 5;
  calf.blue = &cow;

  ox (cow,4,calf,2);
  return;
}

/* { dg-final { scan-tree-dump "base: z, remove_param" "eipa_sra"  } } */
/* { dg-final { scan-tree-dump "base: calf, remove_param" "eipa_sra"  } } */
/* { dg-final { cleanup-tree-dump "eipa_sra" } } */
