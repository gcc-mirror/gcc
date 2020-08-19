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

uint8_t shift[256];

static size_t
__attribute__ ((noclone, noinline))
hash2(const unsigned char *p)
{
  return (((size_t)(p)[0] - ((size_t)(p)[-1] << 3)) % sizeof (shift));
}

char *
simple_strstr (const char *haystack, const char *needle)
{
  const unsigned char *hs = (const unsigned char *) haystack;
  const unsigned char *ne = (const unsigned char *) needle;
  size_t ne_len = strlen ((const char*)ne);
  size_t hs_len = strnlen ((const char*)hs, ne_len | 512);

  if (hs_len < ne_len)
    return NULL;

  if (memcmp (hs, ne, ne_len) == 0)
    return (char *) hs;

  const unsigned char *end = hs + hs_len - ne_len;
  size_t tmp, shift1;
  size_t m1 = ne_len - 1;
  size_t offset = 0;

  memset (shift, 0, sizeof (shift));
  for (int i = 1; i < m1; i++)
    shift[hash2 (ne + i)] = i;
  shift1 = m1 - shift[hash2 (ne + m1)];
  shift[hash2 (ne + m1)] = m1;

  while (1)
    {
      if (__builtin_expect (hs > end, 0))
	{
	  end += strnlen ((const char*)end + m1 + 1, 2048);
	  if (hs > end)
	    return NULL;
	}

      do
	{
	  hs += m1;
	  tmp = shift[hash2 (hs)];
	}
      while (tmp == 0 && hs <= end);

      hs -= tmp;
      if (tmp < m1)
	continue;

      if (m1 < 15 || memcmp (hs + offset, ne + offset, 8) == 0)
	{
	  if (memcmp (hs, ne, m1) == 0)
	    return (void *) hs;

	  offset = (offset >= 8 ? offset : m1) - 8;
	}

      hs += shift1;
    }
}

static int
check_result (const char *s1, const char *s2,
	      char *exp_result)
{
  char *result = simple_strstr (s1, s2);
  if (result != exp_result)
    return -1;

  return 0;
}

void
__attribute__ ((noclone, noinline))
check1 (void)
{
  const char s1[] =
    "F_BD_CE_BD_EF_BF_BD_EF_BF_BD_EF_BF_BD_EF_BF_BD_C3_88_20_EF_BF_BD_EF_BF_BD_EF_BF_BD_C3_A7_20_EF_BF_BD";
  const char s2[] = "_EF_BF_BD_EF_BF_BD_EF_BF_BD_EF_BF_BD_EF_BF_BD";
  char *exp_result;

  exp_result = simple_strstr (s1, s2);
  if (check_result (s1, s2, exp_result) != 0)
    abort ();
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

  check1 ();
  return 0;
}
