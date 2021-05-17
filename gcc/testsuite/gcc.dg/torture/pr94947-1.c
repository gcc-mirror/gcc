/* { dg-do run } */
/* { dg-additional-sources "pr94947-2.c" } */
/* { dg-additional-options "-fipa-pta -flto-partition=1to1" } */
/* { dg-prune-output "warning: using serial compilation" } */

extern void abort ();
extern void baz ();
extern void (*baz_call)();
static int *p;

static void foo ()
{
  if (*p != 1)
    abort ();
}

int main()
{
  int x = 1;
  p = &x;
  baz_call = foo;
  baz ();
  return 0;
}
