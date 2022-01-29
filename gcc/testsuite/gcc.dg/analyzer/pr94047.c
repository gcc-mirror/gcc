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
  struct list l; /* { dg-message "region created on stack here" } */
  tlist t = l; /* { dg-warning "use of uninitialized value 'l'" } */
  for (;;)
    bar (&t);
}
