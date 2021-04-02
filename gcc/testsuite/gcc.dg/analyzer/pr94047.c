typedef struct list
{
  struct list *next;
} tlist;

void
bar (struct list *l)
{
  l->next = l->next->next;
}

void
foo (void)
{
  struct list l;
  tlist t = l;
  for (;;)
    bar (&t);
}
