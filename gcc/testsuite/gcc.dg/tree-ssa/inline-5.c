/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized" } */
struct a {int a,b,c,d,e,f;};

do_inc (struct a *a)
{
  a->a=a->b;
  a->b=a->c;
  a->c=a->d;
  a->e=a->f;
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
/* { dg-final { scan-tree-dump-times "do_inc" 12 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
