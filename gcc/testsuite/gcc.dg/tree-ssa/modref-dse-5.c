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
int
wrap(int b, struct a *a)
{
   kill_me (a);
   return b;
}
__attribute__ ((noinline))
void
my_pleasure (struct a *a)
{
  a->a=1;
  a->c=2;
}
__attribute__ ((noinline))
int
wrap2(int b, struct a *a)
{
   my_pleasure (a);
   return b;
}

int
set (struct a *a)
{
  wrap (0, a);
  int ret = wrap2 (0, a);
  a->b=1;
  return ret;
}
/* { dg-final { scan-tree-dump "Deleted dead store: wrap" "dse1" } } */
