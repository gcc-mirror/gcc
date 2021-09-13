/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse-details" } */

struct z {
  int a;
  int b;
  int c;
};

int
foo(int cond, struct z *s)
{

  if (cond)
    {
      s->a = 1;
      s->b = 2;
      s->c = 3;
    }
  __builtin_memset (s, 0, sizeof (struct z));
}

/* { dg-final { scan-tree-dump-times "Deleted dead store" 3 "dse1"} } */
/* { dg-final { scan-tree-dump-not "Deleted dead store" "dse2"} } */
/* { dg-final { scan-tree-dump-not "Deleted dead store" "dse3"} } */
/* { dg-final { scan-tree-dump-not "Deleted dead store" "dse5"} } */

