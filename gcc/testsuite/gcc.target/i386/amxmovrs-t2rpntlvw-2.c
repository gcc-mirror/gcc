/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target amx_movrs } */
/* { dg-require-effective-target amx_transpose } */
/* { dg-require-effective-target avx512fp16 } */
/* { dg-options "-O2 -mamx-movrs -mamx-transpose -mavx512fp16 -mavx512bf16" } */
#define AMX_MOVRS
#define AMX_TRANSPOSE
#define DO_TEST test_amx_movrs_t2rpntlvw
void test_amx_movrs_t2rpntlvw ();
#include "amx-helper.h"

#define init_pair_tile_reg_and_src_z_t1(tmm_num, src, buffer, ztype, wtype)\
{									   \
  init_pair_tile_src (tmm_num, &src, buffer, ztype);			   \
  _tile_2rpntlvwz##ztype##wtype (tmm_num, buffer, _STRIDE);\
}

void test_amx_movrs_t2rpntlvw ()
{
  __tilecfg_u cfg;
  __tilepair src;
  __tile ref_0, ref_1;
  uint8_t buffer[2048];
  int i;

  init_tile_config (&cfg);

  for (i = 0; i < 2048; i++)
    buffer[i] = i % 256;

  /* Check t2rpntlvwz0.  */
  init_pair_tile_reg_and_src_z_t1 (0, src, buffer, 0,);
  _tile_stored (0, ref_0.buf, _STRIDE);
  _tile_stored (1, ref_1.buf, _STRIDE);
  if (!check_pair_tile_register (&ref_0, &ref_1, &src))
    abort ();

  /* Check t2rpntlvwz1.  */
  init_pair_tile_reg_and_src_z_t1 (1, src, buffer, 1,);
  _tile_stored (0, ref_0.buf, _STRIDE);
  _tile_stored (1, ref_1.buf, _STRIDE);
  if (!check_pair_tile_register (&ref_0, &ref_1, &src))
    abort ();

  /* Check t2rpntlvwz0t1.  */
  init_pair_tile_reg_and_src_z_t1 (0, src, buffer, 0, t1);
  _tile_stored (0, ref_0.buf, _STRIDE);
  _tile_stored (1, ref_1.buf, _STRIDE);
  if (!check_pair_tile_register (&ref_0, &ref_1, &src))
    abort ();

  /* Check t2rpntlvwz1t1.  */
  init_pair_tile_reg_and_src_z_t1 (1, src, buffer, 1, t1);
  _tile_stored (0, ref_0.buf, _STRIDE);
  _tile_stored (1, ref_1.buf, _STRIDE);
  if (!check_pair_tile_register (&ref_0, &ref_1, &src))
    abort ();
}
