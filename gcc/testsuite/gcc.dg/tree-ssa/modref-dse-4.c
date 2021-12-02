/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1-details"  } */
struct a {int a,b,c;};
__attribute__ ((noinline))
void
kill_me (struct a *a)
{
  a->a=0;
  a->b=0;
  a->c=0;
}
__attribute__ ((noinline))
void
my_pleasure (struct a *a)
{
  a->a=1;
  a->c=2;
}
void
set (struct a *a)
{
  kill_me (a);
  my_pleasure (a);
  a->b=1;
}
/* { dg-final { scan-tree-dump "Deleted dead store: kill_me" "dse1" } } */
