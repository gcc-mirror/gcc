#include <stdio.h>
#include <stdint.h>

struct S {
    int a, b;
};

__attribute__((noinline))
void foo (struct S *s) {
    struct S ss = (struct S) {
        .a = s->b,
        .b = s->a
    };
    *s = ss;
}

int main() {
  struct S s = {6, 12};
  foo(&s);
  if (s.a != 12 || s.b != 6)
    __builtin_abort ();
  return 0;
}
