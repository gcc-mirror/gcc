/* The z13 stpcpy implementation plays some alignment tricks for good
   performance.  This test tries to make sure it works correctly and
   does not access bytes beyond the source and destination
   strings.  */

/* { dg-do run } */
/* { dg-require-effective-target vector } */
/* { dg-options "-O3 -mzarch -march=z13" } */

#include <stdio.h>
#include <sys/mman.h>

#define PAGE_SIZE 4096

struct {
  char unused[PAGE_SIZE - 32];
  char m32[15]; /* page bndry - 32 */
  char m17[1];
  char m16[1];
  char m15[14];
  char m1[1];
  char next_page[PAGE_SIZE];
} s, d __attribute__((aligned(PAGE_SIZE)));

char *__attribute__((noinline))
my_stpcpy(char *dest, const char *src)
{
  return __builtin_stpcpy (dest, src);
}

void __attribute__ ((noinline))
check (char *dest, char *src, size_t len)
{
  char *result;

  result = my_stpcpy (dest, src);
  if (result != dest + len)
    __builtin_abort ();
  if (__builtin_memcmp (src, dest, len) != 0)
    __builtin_abort ();
}

int
main ()
{
  char *src[5] = { s.m32, s.m17, s.m16, s.m15, s.m1 };
  char *dst[5] = { d.m32, d.m17, d.m16, d.m15, d.m1 };
  int len[8] = { 33, 32, 31, 17, 16, 15, 1, 0 };
  int i, j, k;
  char backup;

  for (i = 0; i < sizeof (s); i++)
    ((char*)&s)[i] = i % 26 + 97;

  for (i = 0; i < 5; i++)
    for (j = 0; j < 5; j++)
      for (k = 0; k < 8; k++)
	{
	  backup = src[j][len[k]];
	  src[j][len[k]] = 0;
	  __builtin_memset (&d, 0, sizeof (d));
	  check (dst[i], src[j], len[k]);
	  src[j][len[k]] = backup;
	}

  /* Make all source strings end before the page boundary.  */
  backup = s.m1[0];
  s.m1[0] = 0;

  if (mprotect (&s.next_page, PAGE_SIZE, PROT_NONE) == -1)
    perror ("mprotect src");

  for (i = 0; i < 5; i++)
    for (j = 0; j < 5; j++)
      check (dst[i], src[j],
	     PAGE_SIZE - ((unsigned long)src[j] & ((1UL << 12) - 1)) - 1);

  if (mprotect (&s.next_page, PAGE_SIZE, PROT_READ | PROT_WRITE) == -1)
    perror ("mprotect src");

  s.m1[0] = backup;

  if (mprotect (&d.next_page, PAGE_SIZE, PROT_NONE) == -1)
    perror ("mprotect dst");

  for (i = 0; i < 5; i++)
    for (j = 0; j < 5; j++)
      {
	int len = PAGE_SIZE - ((unsigned long)dst[i] & ((1UL << 12) - 1)) - 1;
	char backup = src[j][len];

	src[j][len] = 0;
	__builtin_memset (&d, 0,
			  (unsigned long)&d.next_page - (unsigned long)&d);
	check (dst[i], src[j], len);
	src[j][len] = backup;
      }

  return 0;
}
