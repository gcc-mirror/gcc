/* { dg-options "-O2" } */

typedef struct foo
{
  struct foo **Node;
} foo;

static int sort_and_split (foo **Root, foo **Finite, long first)
{
  foo *cd;
  long i;
  for (i = 0; i < first; i++)
    cd->Node[i] = Finite[first+i];

  sort_and_split(Root, Finite, first);
  return (0);
}


void Build_foo(foo **Root, foo **Finite, foo **Infinite)
{
  long low, high;
  while (sort_and_split(Root, Finite, low) == 0);
}

