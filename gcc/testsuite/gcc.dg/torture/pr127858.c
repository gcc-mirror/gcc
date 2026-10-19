/* { dg-do run } */

struct node {
    struct node *prev;
    struct node *next;
};

struct owner {
    int          id;
    struct node  list;
};

int __attribute__((noipa))
count_stale_reads(struct owner *owner, struct node *list_alias,
		  struct node *new_values, int iterations)
{
  struct node **field = &owner->list.prev;
  int stale = 0;
  int i;

  for (i = 0; i < iterations; i++)
    {
      struct node *before = owner->list.prev;
      list_alias->prev = &new_values[i];
      struct node *after = *field;
      if (before == after)
	stale++;
    }

  return stale;
}

int
main()
{
  static struct owner  owner;          /* list.prev starts out NULL */
  static struct node   new_values[4];
  int                  stale;

  stale = count_stale_reads (&owner, &owner.list, new_values, 4);
  if (stale != 0)
    __builtin_abort ();
  return 0;
}
