/* { dg-do compile { target { nonpic } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
static int a;
int f;
void bar (void)  __attribute__((noinline));
void bar (void)
{
  f = 9;
}

void link_error ();

int foo()
{
  int b, c;
  a = 5;
  b = a;
  bar ();
  b = b + a;
  if (b != 10)
    link_error ();
  return b;
}

/* We should have removed the link_error on the tree level as GCC can tell that
   a is not touched by the calling bar at all. */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"} } */
