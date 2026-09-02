#ifndef ACE_CHECK_H_INCLUDED
#define ACE_CHECK_H_INCLUDED
#include "cpuid.h"
#include "m512-check.h"

typedef struct __tile_config
{
  unsigned char palette_id; 
  unsigned char reserved[63];
} __tilecfg;

typedef union __tile
{
  unsigned char buf[1024];
  float a[256];
  int b[256];
} __tile;

typedef struct __bsr
{
  unsigned char buf[128];
} __bsr;

void init_bsr (__bsr *bsr, union512i_ub *src1, union512i_ub *src2)
{
  int i;
  for (i = 0; i < 64; i++)
    {
      bsr->buf[i] = 0x7f;
      src1->a[i] = 0x7f;
    }
  for (i = 0; i < 64; i++)
    {
      bsr->buf[i + 64] = 0x7f;
      src2->a[i] = 0x7f;
    }
}

void fill_bsr (__bsr *bsr, union512i_ub* src1, union512i_ub* src2)
{
  int i;
  for (i = 0; i < 64; i++)
    {
      bsr->buf[i] = 127 + i;
      src1->a[i] = 127 + i;
    }
  for (i = 0; i < 64; i++)
    {
      bsr->buf[i + 64] = 127 - i;
      src2->a[i] = 127 - i;
    }
}

#ifndef DO_TEST
#define DO_TEST do_test
static void test_ace (void);
__attribute__ ((noinline))
static void
do_test (void)
{
  test_ace ();
}
#endif

int
main ()
{
  /* Check cpu support for ACE */
  if (__builtin_cpu_supports ("acev1"))
    {
      DO_TEST ();
#ifdef DEBUG
      printf ("PASSED\n");
#endif
    }
#ifdef DEBUG
  else
    printf ("SKIPPED\n");
#endif

  return 0;
}

#endif
