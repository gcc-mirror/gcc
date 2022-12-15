// { dg-do compile }
// { dg-options "-O -fno-tree-dominator-opts -fno-tree-fre -Wmaybe-uninitialized" }

void printf(...);
void __sigsetjmp_cancel() __attribute__((__returns_twice__));
int z, main_ret;
void func(void *) {}

int
main()
{
  int x;
  void (*__cancel_routine)(void *)(func);
  __sigsetjmp_cancel();
  __cancel_routine(0);
  if (main_ret)
    x = z;
  printf(x);
}
