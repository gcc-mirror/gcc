/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-skip-if "no stdlib.h in eBPF" { bpf-*-* } } */
#include <stdlib.h>
struct s {
  unsigned short f: 16;
  unsigned short y: 8;
  unsigned short g: 2;
  unsigned int x;
};

void set (struct s*, int) __attribute__((noinline));
void set (struct s* p, int flags) {
  p->g = flags << 1;
}

int
main() {
  struct s foo = {0 , 0, 3, 0};
  set (&foo, -1);
  if (foo.g != 2)
    abort();
  return 0;
}
