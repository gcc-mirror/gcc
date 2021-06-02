/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-forwprop --param=evrp-mode=ranger -fcompare-debug  " } */

extern void __attribute__((noreturn)) error();

int x;

static inline int bar(void) {
  char n = 1;
  int i = x & 1U << n - 1;
  return i;
}

void foo()
{
  int a = bar();
  for (;;) {
    bool b;
    int d = a;
    b = a < 2;
    if (!b)
      error();
  }
}
