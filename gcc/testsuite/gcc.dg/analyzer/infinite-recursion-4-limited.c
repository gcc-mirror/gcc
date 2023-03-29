/* { dg-additional-options "-fno-analyzer-call-summaries -Wno-analyzer-too-complex" } */

/* A two-deep mutual recursion, walking a singly-linked list,
   with a depth limit.  */

struct node
{
  struct node *child;
};

void foo (struct node *f, int depth);

void bar (struct node *b, int depth)
{
  foo (b, depth); /* { dg-bogus "infinite recursion" } */
}

void foo (struct node *f, int depth)
{
  if (f->child && depth > 0)
    bar (f->child, depth - 1); /* { dg-bogus "infinite recursion" } */
}
