/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

struct s { int x; };
struct t { int x; };

void swap(struct s* p, struct t* q)
{
  p->x = q->x;
  q->x = p->x;
}

/* The second statement is redundant.  */
/* { dg-final { scan-tree-dump-times "x = " 1 "fre1" { xfail { ! natural_alignment_32 } } } } */
/* { dg-final { scan-tree-dump-times " = \[^;\]*x;" 1 "fre1" } } */
