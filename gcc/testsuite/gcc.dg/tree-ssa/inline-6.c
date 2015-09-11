/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized" } */
struct a {int a,b,c,d,e,f,g,h,i,j;};
void do_something (struct a *);

void
do_inc (struct a *a)
{
  a->a=1;
  a->b=2;
  a->c=3;
  a->e=4;
  a->f=5;
  a->g=5;
  a->h=5;
  a->i=5;
  a->j=5;
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
