/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimize" } */
struct a {int a,b,c,d,e,f;};

do_inc (struct a *a)
{
  a->a=1;
  a->b=2;
  a->c=3;
  a->e=4;
  a->f=5;
}

test(struct a *a)
{
  do_inc (a);
  do_something (a);
  do_inc (a);
  do_something (a);
  do_inc (a);
  do_something (a);
  do_inc (a);
  do_something (a);
  do_inc (a);
  do_something (a);
  do_inc (a);
  do_something (a);
  do_inc (a);
  do_something (a);
  do_inc (a);
  do_something (a);
  do_inc (a);
  do_something (a);
}
/* { dg-final { scan-tree-dump-times "do_inc.*;" 10 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
