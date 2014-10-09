/* { dg-do run } */

extern void abort (void);

struct node { struct node *next, *prev; } node;
struct head { struct node *first; } heads[5];
int k = 2;
struct head *head = &heads[2];

static int __attribute__((noinline))
foo()
{
  node.prev = (void *)head;
  head->first = &node;

  struct node *n = head->first;
  struct head *h = &heads[k];

  if (n->prev == (void *)h)
    h->first = n->next;
  else
    n->prev->next = n->next;

  n->next = h->first;
  return n->next == &node;
}

int main()
{
  if (foo ())
    abort ();
  return 0;
}
