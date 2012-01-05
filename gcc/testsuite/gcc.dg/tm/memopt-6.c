/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O -fdump-tree-tmedge --param tm-max-aggregate-size=1" } */

struct large { int x[100]; };
struct large bark();
extern int test (void) __attribute__((transaction_safe));
struct large lacopy;

int f()
{
  int i = readint();
  struct large lala = bark();
  __transaction_atomic {
    lala.x[55] = 666;
    lala = lacopy;		/* Aggregate instrumentation.  */
  }
  return lala.x[i];
}

/* { dg-final { scan-tree-dump-times "memmoveRtWt \\\(.*, &lacopy" 1 "tmedge" } } */
/* { dg-final { cleanup-tree-dump "tmedge" } } */
