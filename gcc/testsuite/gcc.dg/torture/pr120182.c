/* { dg-do run { target { { *-*-linux* *-*-gnu* *-*-uclinux* } && mmap } } } */

#include <unistd.h>
#include <stdlib.h>
#include <sys/mman.h>

struct S
{
  struct S *next;
};

static void __attribute__((noipa))
allocate(void *addr, unsigned long long size)
{
  void *ptr = mmap((void *)addr, size,
		   PROT_READ | PROT_WRITE,
		   MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED_NOREPLACE,
		   -1, 0);
  if(ptr != addr)
    exit(0);
}

int main (void)
{
  int size = 0x8000;
  char *ptr = (char *)0x288000ull;
  allocate((void *)ptr, size);

  struct S *s1 = (struct S *)ptr;
  struct S *s2 = (struct S *)256;
  for (int i = 0; i < 3; i++)
    {
      for(char *addr = (char *)s1; addr < (char *)s1 + sizeof(*s1); ++addr)
	*addr = 0;

      if(s1->next)
	s1->next = s1->next->next = s2;
      else
	s1->next = s2;
    }
  return 0;
}
