/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target acev1 } */
/* { dg-options "-O2 -macev1" } */
#define DO_TEST test_acev1_bsrmovl
void test_acev1_bsrmovl ();
#include "ace-helper.h"

void test_acev1_bsrmovl ()
{
  __tilecfg cfg;
  __bsr bsr0;
  union512i_ub src1, src2, res;
  int i, miss;

  init_tile_config (&cfg, &bsr0);

  fill_bsr (&bsr0, &src1, &src2);

  _bsr0_insertl (src1.x);
  res.x = _bsr0_extractl ();

  miss = 0;
  for (i = 0; i < 64; i++)
    if (res.a[i] != bsr0.buf[i])
      {
#ifdef DEBUG
	printf ("%d: %d != %d\n", i, res.a[i], bsr0.buf[i]);
#endif
	miss++;
      }

  if (miss)
    abort ();
}
