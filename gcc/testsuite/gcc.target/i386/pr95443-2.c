/* { dg-do run { target mmap } } */
/* { dg-options "-O2 -minline-all-stringops" } */

#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#ifndef MAP_ANON
#define MAP_ANON 0
#endif
#ifndef MAP_FAILED
#define MAP_FAILED ((void *)-1)
#endif

int ret;

static void
do_one_test (char *dst, char *src, const char *orig_src, unsigned int len)
{
  __builtin_memcpy (src, orig_src, len);
  __builtin_memmove (dst, src, len);

  if (__builtin_memcmp (dst, orig_src, len) != 0)
    {
      ret = 1;
      return;
    }
}

void
do_test (char *s1, char *s2, int n, unsigned int len)
{
  int i;
  for (i = 0; i < n; i++)
    do_one_test (s2, s2, s1, len);
}

int
main (void)
{
  unsigned char *buf1, *buf2;
  size_t page_size = 2 * sysconf(_SC_PAGESIZE);

  buf1 = mmap (0, (1 + 1) * page_size, PROT_READ | PROT_WRITE,
	       MAP_PRIVATE | MAP_ANON, -1, 0);
  if (buf1 == MAP_FAILED)
    return -1;
  if (mprotect (buf1 + 1 * page_size, page_size, PROT_NONE))
    return -1;
  buf2 = mmap (0, 2 * page_size, PROT_READ | PROT_WRITE,
	       MAP_PRIVATE | MAP_ANON, -1, 0);
  if (buf2 == MAP_FAILED)
    return -1;
  if (mprotect (buf2 + page_size, page_size, PROT_NONE))
    return -1;

  memset (buf1, 0xa5, 1 * page_size);
  memset (buf2, 0x5a, page_size);

  char *s1 = (char *) buf1;
  char *s2 = (char *) buf2;

  size_t len;
  size_t i, j;
  len = 1 << 2;
  for (i = 0, j = 1; i < len; i++, j += 23)
    s1[i] = j;

  do_test (s1, s2, 10, 1 << 2);

  len = 1 << 4;
  for (i = 0, j = 1; i < len; i++, j += 23)
    s1[i] = j;

  do_test (s1, s2, 10, 1 << 4);

  return ret;
}
