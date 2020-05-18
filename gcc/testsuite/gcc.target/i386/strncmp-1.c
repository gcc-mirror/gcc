/* { dg-do run { target mmap } } */
/* { dg-options "-O2" } */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif

int
__attribute__ ((noclone, noinline))
compare (char *d, char *s, unsigned int l)
{
  return __builtin_strncmp (d, s, l);
}

int
main ()
{
  size_t page_size = sysconf(_SC_PAGESIZE);
  char *buf = mmap (0, 2 * page_size, PROT_READ | PROT_WRITE,
		    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (buf == MAP_FAILED)
    {
      perror ("mmap");
      abort ();
    }

  if (mprotect (buf + page_size, page_size, PROT_NONE))
    {
      perror ("mprotect");
      abort ();
    }

  char *src1 = buf + page_size - sizeof ("foo");
  char *src2 = buf;
  memcpy (src1, "foo", sizeof ("foo"));
  memcpy (src2, "foo", sizeof ("foo"));
  int result = compare (src1, src2, sizeof ("foo") + 16);
  if (result != 0)
    abort ();
  return 0;
}
