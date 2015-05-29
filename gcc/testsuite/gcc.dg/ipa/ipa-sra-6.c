/* { dg-do compile } */
/* { dg-options "-O2 -fipa-sra -fdump-tree-eipa_sra-slim"  } */
/* { dg-require-effective-target non_strict_align } */

struct bovid
{
  float a;
  int b;
  struct bovid *next;
};

static int
__attribute__((noinline))
foo (struct bovid *cow, int i)
{
  i++;
  if (cow->next)
    foo (cow->next, i);
  return i;
}

int main (int argc, char *argv[])
{
  struct bovid cow;

  cow.a = 7.4;
  cow.b = 6;
  cow.next = (struct bovid *) 0;

  return foo (&cow, 0);
}

/* { dg-final { scan-tree-dump-times "foo " 1 "eipa_sra"  } } */
