/* { dg-additional-options "-fno-analyzer-call-summaries -Wno-analyzer-too-complex" } */

/* A two-deep mutual recursion, walking a linked list (and thus presumably
   terminating), with no explicit depth limit.  */

struct node
{
  struct node *child;
};

void foo (struct node *f);

void bar (struct node *b)
{
  foo (b); /* { dg-bogus "infinite recursion" } */
}

void foo (struct node *f)
{
  if (f->child)
    bar (f->child); /* { dg-bogus "infinite recursion" } */
}
