/* { dg-do compile } */
/* { dg-options "-O2 -fno-early-inlining -fno-tree-vrp -fno-tree-dominator-opts -fdump-tree-thread2-details-blocks" } */

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
/* { dg-final { scan-tree-dump-times "Registering jump thread" 2 "thread2" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "thread2" } } */
