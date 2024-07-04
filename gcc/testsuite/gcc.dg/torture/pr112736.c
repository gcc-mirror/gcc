/* { dg-do run { target *-*-linux* *-*-gnu* *-*-uclinux* } } */

#include <sys/mman.h>
#include <unistd.h>

int a, c[3][5];

void __attribute__((noipa))
fn1 (int * __restrict b)
{
  int e;
  for (a = 2; a >= 0; a--)
    for (e = 0; e < 4; e++)
      c[a][e] = b[a];
}

int main()
{
  long pgsz = sysconf (_SC_PAGESIZE);
  void *p = mmap (NULL, pgsz * 2, PROT_READ|PROT_WRITE,
                  MAP_ANONYMOUS|MAP_PRIVATE, 0, 0);
  if (p == MAP_FAILED)
    return 0;
  mprotect (p, pgsz, PROT_NONE);
  fn1 (p + pgsz);
  return 0;
}
