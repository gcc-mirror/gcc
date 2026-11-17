/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

struct s { int x; };
struct t { int x; };

void swap(struct s* p, struct t* q)
{
  p->x = q->x;
  q->x = p->x;
}

/* The second statement is _not_ redundant, it may change the dynamic
   type back from struct s to struct t.  */
/* { dg-final { scan-tree-dump-times "x = " 2 "fre1" } } */
/* { dg-final { scan-tree-dump-times " = \[^;\]*x;" 1 "fre1" } } */
