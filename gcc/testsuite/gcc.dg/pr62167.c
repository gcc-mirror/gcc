/* { dg-do compile } */
/* { dg-options "-O2 -ftree-tail-merge -fdump-tree-pre" } */

struct node
{
  struct node *next;
  struct node *prev;
};

struct node node;

struct head
{
  struct node *first;
};

struct head heads[5];

int k = 2;

struct head *head = &heads[2];

int
main ()
{
  struct node *p;

  node.next = (void*)0;

  node.prev = (void *)head;

  asm("":"=m"(node.prev));

  head->first = &node;

  struct node *n = head->first;

  struct head *h = &heads[k];

  heads[2].first = n->next;

  if ((void*)n->prev == (void *)h)
    p = h->first;
  else
    /* Dead tbaa-unsafe load from ((struct node *)&heads[2])->next.  */
    p = n->prev->next;

  return !(p == (void*)0);
}

/* { dg-final { scan-tree-dump-not "Removing basic block" "pre"} } */
