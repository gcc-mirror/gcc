/* { dg-do compile } */
/* { dg-options "-O2 -fipa-sra -fdump-tree-eipa_sra-details"  } */

struct bovid
{
  float red;
  int green;
  void *blue;
};

static int
__attribute__((noinline))
ox (struct bovid *cow)
{
  cow->red = cow->red + cow->green + cow->green;
  return 0;
}

int something;

static int
__attribute__((noinline))
ox_improved (struct bovid *calf)
{
  if (something > 0)
    calf->red = calf->red + calf->green;
  else
    calf->red = calf->green + 87;
  something = 77;
  return 0;
}


int main (int argc, char *argv[])
{
  struct bovid cow;

  cow.red = 7.4;
  cow.green = 6;
  cow.blue = &cow;

  ox (&cow);

  ox_improved (&cow);
  return 0;
}

/* { dg-final { scan-tree-dump "About to replace expr cow_.*D.->red with \\*ISRA" "eipa_sra"  } } */
/* { dg-final { scan-tree-dump "About to replace expr cow_.*D.->green with ISRA" "eipa_sra"  } } */
/* { dg-final { scan-tree-dump "About to replace expr calf_.*D.->red with \\*ISRA" "eipa_sra"  } } */
/* { dg-final { scan-tree-dump "About to replace expr calf_.*D.->green with ISRA" "eipa_sra"  } } */
