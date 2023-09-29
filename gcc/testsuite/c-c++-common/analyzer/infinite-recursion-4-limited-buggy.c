/* { dg-additional-options "-fno-analyzer-call-summaries -Wno-analyzer-too-complex" } */

/* A two-deep mutual recursion, and failing to walk a list,
   but with a depth limit, thus not an infinite recursion (assuming a
   suitable depth limit).  */

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
    /* Bug: should have recursed to f->child, not to f,
       but we assume that the depth limit should save us.  */
    bar (f, depth - 1); /* { dg-bogus "infinite recursion" } */
}
