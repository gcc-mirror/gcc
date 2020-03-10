/* { dg-additional-options "-Wno-analyzer-too-complex" } */
/* TODO: the above ought not to be necessary, but currently is due to a
   state explosion within the for loop.  */

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
