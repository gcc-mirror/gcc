/* { dg-additional-options "-fno-analyzer-call-summaries -Wno-analyzer-too-complex" } */

/* A two-deep mutual recursion, with no limit, and
   failing to walk the list, thus leading to an infinite recursion.  */

struct node
{
  struct node *child;
};

void foo (struct node *f);

void bar (struct node *b)
{
  foo (b); /* { dg-warning "infinite recursion" } */
}

void foo (struct node *f)
{
  if (f->child)
    /* Bug: should have recursed to f->child, not to f.  */
    bar (f); /* { dg-warning "infinite recursion" } */
}
