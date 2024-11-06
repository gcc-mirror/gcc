/* { dg-do run { target { ! ia32  }  }  } */
/* { dg-require-effective-target amx_movrs } */
/* { dg-options "-O2 -mamx-movrs"  } */
#define AMX_MOVRS
#define DO_TEST test_amx_movrs_tileloaddrs
void test_amx_movrs_tileloaddrs ();
#include "amx-helper.h"

void test_amx_movrs_tileloaddrs ()
{
  __tilecfg_u cfg;
  __tile reg_src0, reg_src1, reg_ref0, reg_ref1;
  uint8_t buffer[1024];
  int i;

  for (i = 0; i < 1024; i++)
    buffer[i] = i % 256;


  init_tile_config (&cfg);

  init_tile_src (0, &reg_src0, buffer);
  _tile_loaddrs (0, reg_src0.buf, _STRIDE);
  _tile_stored (0, reg_ref0.buf, _STRIDE);
  if (!check_tile_register (&reg_ref0, &reg_src0))
    abort();

  init_tile_src (1, &reg_src1, buffer);
  _tile_loaddrst1 (1, reg_src1.buf, _STRIDE);
  _tile_stored (1, reg_ref1.buf, _STRIDE);
  if (!check_tile_register (&reg_ref1, &reg_src1))
    abort();
}
