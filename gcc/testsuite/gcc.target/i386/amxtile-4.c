/* PR target/114098 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mamx-tile" } */

#include <stdint.h>
#include <x86intrin.h>

#define MAX_ROWS 16
#define MAX_COLS 64
#define MAX 1024
#define STRIDE 64

typedef struct __tile_config
{
  uint8_t palette_id;
  uint8_t start_row;
  uint8_t reserved_0[14];
  uint16_t colsb[16];
  uint8_t rows[16];
} __tilecfg __attribute__ ((aligned (64)));

/* Initialize tile config */
static void
init_tile_config (__tilecfg *tileinfo)
{
  int i;
  tileinfo->palette_id = 1;
  tileinfo->start_row = 0;

  for (i = 0; i < 1; ++i)
  {
    tileinfo->colsb[i] = MAX_ROWS;
    tileinfo->rows[i] = MAX_ROWS;
  }

  for (i = 1; i < 4; ++i)
  {
    tileinfo->colsb[i] = MAX_COLS;
    tileinfo->rows[i] = MAX_ROWS;
  }

  _tile_loadconfig (tileinfo);
}

void
enable_amx (void)
{
  __tilecfg tile_data = {0};
  init_tile_config (&tile_data);
}

/* { dg-final { scan-assembler-times "pxor\[^\n\]*%xmm" 1 } } */
