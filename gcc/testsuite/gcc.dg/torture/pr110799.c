/* { dg-do run { target { { *-*-linux* *-*-gnu* *-*-uclinux* } && mmap } } } */

#include <unistd.h>
#include <stdlib.h>
#include <sys/mman.h>

struct S {
    int a;
};
struct M {
    int a, b;
};

int __attribute__((noipa))
f(struct S *p, int c, int d)
{
  int r;
  if (c)
    {
      if (d)
	r = p->a;
      else
	r = ((struct M*)p)->a;
    }
  else
    r = ((struct M*)p)->b;
  return r;
}

int main ()
{
  long pgsz = sysconf(_SC_PAGESIZE);
  if (pgsz < sizeof (struct M))
    return 0;
  char *p = mmap ((void *) 0, 2 * pgsz, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS,
		  -1, 0);
  if (p == MAP_FAILED)
    return 0;
  if (mprotect (p, pgsz, PROT_READ | PROT_WRITE))
    return 0;
  struct S *q = (struct S *)(p + pgsz) - 1;
  q->a = 42;
  if (f (q, 1, 1) != 42)
    abort ();
  return 0;
}
