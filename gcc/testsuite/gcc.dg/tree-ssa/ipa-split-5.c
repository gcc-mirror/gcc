/* { dg-do compile { target nonpic } } */
/* { dg-options "-O3 -fdump-tree-fnsplit -fdump-tree-optimized --param=builtin-expect-probability=100" } */

struct a {int a,b;};
struct a make_me_big (int a);
struct a split_me (int a)
{
  struct a retval;
  if (__builtin_expect (a!=0,1))
    {
      retval.a = 0;
      retval.b = 0;
      return retval;
    }
  else
    {
      struct a retval = make_me_big (a);
      retval = make_me_big (a);
      retval = make_me_big (a);
      retval = make_me_big (a);
      retval = make_me_big (a);
      retval = make_me_big (a);
      return retval;
    }
}
int val;
test()
{
  split_me (val);
  split_me (val);
  split_me (val);
  split_me (val);
}
/* { dg-final { scan-tree-dump-times "Splitting function" 1 "fnsplit"} } */
/* { dg-final { cleanup-tree-dump "fnsplit" } } */
/* { dg-final { scan-tree-dump "part" "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
