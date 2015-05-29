/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized" } */
struct a {int a,b,c,d,e,f;};

void do_something (struct a *);

void
do_inc (struct a *a)
{
  a->a=a->b;
  a->b=a->c;
  a->c=a->d;
  a->e=a->f;
}

void
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
