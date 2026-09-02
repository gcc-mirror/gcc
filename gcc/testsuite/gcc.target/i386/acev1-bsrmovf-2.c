/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target acev1 } */
/* { dg-options "-O2 -macev1" } */
#define DO_TEST test_acev1_bsrmovf
void test_acev1_bsrmovf ();
#include "ace-helper.h"

void test_acev1_bsrmovf ()
{
  __tilecfg cfg;
  __bsr bsr0;
  union512i_ub src1, src2, res1, res2;
  int i, miss;

  init_tile_config (&cfg, &bsr0);

  fill_bsr (&bsr0, &src1, &src2);

  _bsr0_insertfull (src2.x, src1.x);
  res1.x = _bsr0_extractl ();
  res2.x = _bsr0_extracth ();

  miss = 0;
  for (i = 0; i < 64; i++)
    if (res1.a[i] != bsr0.buf[i])
      {
#ifdef DEBUG
	printf ("%d: %d != %d\n", i, res1.a[i], bsr0.buf[i]);
#endif
	miss++;
      }

  for (i = 0; i < 64; i++)
    if (res2.a[i] != bsr0.buf[i + 64])
      {
#ifdef DEBUG
	printf ("%d: %d != %d\n", i, res2.a[i], bsr0.buf[i + 64]);
#endif
	miss++;
      }

  if (miss)
    abort ();
}
