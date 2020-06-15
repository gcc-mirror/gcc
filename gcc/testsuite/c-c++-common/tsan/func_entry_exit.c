/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple -fdump-tree-optimized" } */

int x;

__attribute__((noinline))
void fn1(void)
{
  x++;
}

__attribute__((noinline))
void fn2(void)
{
  fn1();
}

__attribute__((noinline))
int main(int argc, char *argv[])
{
  fn1();
  fn2();
  return 0;
}

// { dg-final { scan-tree-dump "TSAN_FUNC_EXIT" "gimple" } }
// { dg-final { scan-tree-dump-times "__tsan_func_entry" 3 "optimized" } }
// { dg-final { scan-tree-dump-times "__tsan_func_exit" 3 "optimized" } }
// { dg-final { scan-tree-dump "__tsan_write" "optimized" } }
