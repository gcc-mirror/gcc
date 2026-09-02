/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target acev1 } */
/* { dg-options "-O2 -macev1" } */
#define DO_TEST test_acev1_movcol
void test_acev1_movcol ();
#include "ace-helper.h"

void calc_movrow (__tile *src, int *dst, int row)
{
  int i, index;

  index = row % 16;
  for (i = 0; i < 16; i++)
    dst[i] = src->b[16 * index + i];
}

void test_acev1_movcol ()
{
  __tilecfg cfg;
  __tile src;
  __bsr bsr0;
  union512i_d res;
  int res_ref[16];
  int i, j;

  init_tile_config (&cfg, &bsr0);
  for (i = 0; i < 16; i++)
  {
    union512i_ud tmp;
    for (j = 0; j < 16; j++)
      {
	tmp.a[j] = i * 16 + j;
	src.b[i + j * 16] = i * 16 + j;
      }
    _tile_insertcol (1, tmp.x, i);
  }

  for (i = 0; i < 16; i++)
    {
      calc_movrow (&src, res_ref, i);
      res.x = _tile_extractrow (1, i);
      if (UNION_CHECK (512, i_d) (res, res_ref))
	abort ();
    }
}
