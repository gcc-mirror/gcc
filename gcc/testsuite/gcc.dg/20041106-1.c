/* { dg-do run { target *-*-linux* *-*-gnu* *-*-solaris* *-*-uclinux* } } */
/* { dg-options -O2 } */

#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>

struct S { long x __attribute__((packed)); }; /* { dg-warning "attribute ignored" "" { target default_packed } } */
volatile long sink;

void foo (struct S *s)
{
  sink = s->x;
}

int main ()
{
  size_t ps = getpagesize ();
  char *ptr, *page;
  struct S *s;

  ptr = malloc (3*ps);
  page = (char *)(((__UINTPTR_TYPE__)ptr + (ps - 1)) & -ps);
  munmap (page + ps, ps);

  s = (struct S *)(page + ps - sizeof(struct S));
  foo(s);

  return 0;
}
