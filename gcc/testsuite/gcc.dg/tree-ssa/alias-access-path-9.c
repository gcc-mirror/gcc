/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

/* This testcase tests nonoverlapping_component_refs_since_match_p in presence
   of non-trivial mem-refs.  */
struct a {int a,b;};
struct b {struct a a[10];};
struct c {int c; struct b b;} c, *cptr;

void
set_a(struct a *a, int p)
{
  a->a=p;
}
void
set_b(struct a *a, int p)
{
  a->b=p;
}
int
get_a(struct a *a)
{
  return a->a;
}

int
test(int i, int j)
{
  struct b *bptr = &c.b;
  set_a (&bptr->a[i], 123);
  set_b (&bptr->a[j], 124);
  return get_a (&bptr->a[i]);
}

int
test2(int i, int j)
{
  struct b *bptr = &cptr->b;
  set_a (&bptr->a[i], 125);
  set_b (&bptr->a[j], 126);
  return get_a (&bptr->a[i]);
}
/* { dg-final { scan-tree-dump-times "return 123" 1 "fre1"} } */
/* { dg-final { scan-tree-dump-times "return 125" 1 "fre1"} } */
