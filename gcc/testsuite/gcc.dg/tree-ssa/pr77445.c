/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-thread3-details-blocks -fno-early-inlining -fno-tree-vrp -fno-tree-dominator-opts" } */

static int a;
static int b;
void test2 ();
void
test ()
{
  b = 7;
}

void
main (int argc)
{
  if (argc)
    {
      a = 7;
      test ();
    }
  else
    a = 0;
  if (a)
    test2 ();
  if (b)
    test2 ();
}
/* { dg-final { scan-tree-dump-times "Registering FSM jump thread" 2 "thread3" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "thread3" } } */
