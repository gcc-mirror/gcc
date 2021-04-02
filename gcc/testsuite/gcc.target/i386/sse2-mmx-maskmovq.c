/* { dg-do run { target { sysconf && mmap } } } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2" } */
/* { dg-additional-options "-mno-mmx" { target { ! ia32 } } } */

#include <unistd.h>
#include <sys/mman.h>
#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif
#include "sse2-check.h"
#include "mmx-vals.h"

__attribute__((noinline, noclone))
static void
test_maskmovq  (long long *ll1, long long *ll2, long long *r)
{
  __m64 t1 = *(__m64 *) ll1;
  __m64 t2 = *(__m64 *) ll2;
   _m_maskmovq (t1, t2, (char *) r);
}

/* Routine to manually compute the results */
static void
compute_correct_result (long long *dst_p, long long *src_p,
			long long *res_p)
{
  char *dst = (char *) dst_p;
  char *src = (char *) src_p;
  char *res = (char *) res_p;
  int i;
  for (i = 0; i < 8; i++)
    if ((src[i] & 0x80) != 0)
      res[i] = dst[i];
}

static void
do_maskmovq_test (long long *r)
{
  int i;
  long long ck;
  int fail = 0;

  /* Run the MMX tests */
  for (i = 0; i < MMX_num_ops; i++)
    {
      r[0] = -1LL;
      ck = -1LL;
      test_maskmovq (&MMXops[i], &MMXops[i], r);
      compute_correct_result (&MMXops[i], &MMXops[i], &ck);
      if (*r != ck)
	fail++;
    }

  if (fail != 0)
    abort ();
}

static void
sse2_test (void)
{
  char *buf;
  long long *r;
  size_t page_size = sysconf(_SC_PAGESIZE);

  buf = mmap (0, 3 * page_size, PROT_READ | PROT_WRITE,
	      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (buf == MAP_FAILED)
    {
      perror ("mmap");
      abort ();
    }

  if (mprotect (buf, page_size, PROT_NONE))
    {
      perror ("mprotect");
      abort ();
    }

  if (mprotect (buf + 2 * page_size, page_size, PROT_NONE))
    {
      perror ("mprotect");
      abort ();
    }

  r = (long long *) (buf + page_size);
  do_maskmovq_test (r);

  r = (long long *) (buf + page_size + 3);
  do_maskmovq_test (r);

  r = (long long *) (buf + page_size + 11);
  do_maskmovq_test (r);

  r = (long long *) (buf + 2 * page_size - 16);
  do_maskmovq_test (r);

  r = (long long *) (buf + 2 * page_size - 16 + 3);
  do_maskmovq_test (r);

  r = (long long *) (buf + 2 * page_size - 16 + 8);
  do_maskmovq_test (r);
}
