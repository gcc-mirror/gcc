/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O -fdump-tree-tmmark" } */

struct large { int x[100]; };
struct large large_global;
extern struct large function (void) __attribute__((transaction_safe));

void f()
{
  __transaction_atomic {
      large_global = function();
  }
}

/* { dg-final { scan-tree-dump-times "memcpyRnWt \\\(&large_global," 1 "tmmark" } } */
