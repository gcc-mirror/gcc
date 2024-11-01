#ifndef AMX_CHECK_H_INCLUDED
#define AMX_CHECK_H_INCLUDED

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/syscall.h>
#endif
#ifdef DEBUG
#include <stdio.h>
#endif
#include "cpuid.h"

#define XFEATURE_XTILECFG	17
#define XFEATURE_XTILEDATA	18
#define XFEATURE_MASK_XTILECFG	(1 << XFEATURE_XTILECFG)
#define XFEATURE_MASK_XTILEDATA	(1 << XFEATURE_XTILEDATA)
#define XFEATURE_MASK_XTILE	(XFEATURE_MASK_XTILECFG | XFEATURE_MASK_XTILEDATA)

#define ARCH_GET_XCOMP_PERM	0x1022
#define ARCH_REQ_XCOMP_PERM	0x1023

/* TODO: The tmm emulation is temporary for current
   AMX implementation with no tmm regclass, should
   be changed in the future. */
typedef struct __tile_config
{
  uint8_t palette_id; 
  uint8_t start_row;   
  uint8_t reserved_0[14];
  uint16_t colsb[8]; /* Colum size of each tmm register in bytes */
  uint16_t reserved_1[8];
  uint8_t rows[8]; /* Row size of each tmm reg in bytes */
  uint8_t reserved_2[8];
} __tilecfg;

typedef union __union_tile_config
{
  __tilecfg s;
  uint8_t a[64];
} __tilecfg_u;

typedef struct __tile
{
  /* Max size of tile register */
  uint8_t buf[1024];
  int rows;
  int colsb;
} __tile;

typedef struct __tilepair
{
  /* Max size of tile register */
  uint8_t buf[2048];
  int rows;
  int colsb;
} __tilepair;

/* Maxium col/row size in bytes */
#define MAX_ROWS 16
#define MAX_COLS 64

/* Stride (colum width in byte) used for tileload/store */
#define _STRIDE 64

#ifdef __linux__
/* We need syscall to use amx functions */
int request_perm_xtile_data()
{
  unsigned long bitmask;

  if (syscall (SYS_arch_prctl, ARCH_REQ_XCOMP_PERM, XFEATURE_XTILEDATA) ||
      syscall (SYS_arch_prctl, ARCH_GET_XCOMP_PERM, &bitmask))
    return 0;

  return (bitmask & XFEATURE_MASK_XTILE) != 0;
}
#endif

/* Initialize tile config by setting all tmm size to 16x64 */
void init_tile_config (__tilecfg_u *dst)
{
  int i;

  dst->s.palette_id = 1;
  dst->s.start_row = 0;

  for (i = 0; i < 14; i++)
    dst->s.reserved_0[i] = 0;

  for (i = 0; i < 8; i++)
  {
    dst->s.colsb[i] = _STRIDE;
    dst->s.rows[i] = 16;
    dst->s.reserved_1[i] = 0;
    dst->s.reserved_2[i] = 0;
  }

  _tile_loadconfig (dst->a);
}

/* Init __tile variable that going to be store to register
   w/o extra buffer. If buffer exists, it should be the same
   size matrix as corresponding tmm register.
   Should execute init_tile_config first */
void init_tile_src (const int tmm_num, __tile *src, uint8_t *buffer)
{
  int rows, colsb, i, j;
  __tilecfg_u tmp;

  _tile_storeconfig (tmp.a);

  src->rows = rows = tmp.s.rows[tmm_num];
  src->colsb = colsb = tmp.s.colsb[tmm_num];

  for (i = 0; i < rows; i++)
    for (j = 0; j < colsb; j++)
    {
      if(buffer)
	src->buf[i * colsb + j] = buffer[i * colsb + j];
      else
	src->buf[i * colsb + j] = (i + 11 * j) % 256;
    }

}

/* Init __tile src and corresponding tmm register */
#define init_tile_reg_and_src(tmm_num, src)   \
{					      \
  init_tile_src (tmm_num, &src, NULL);	      \
  _tile_loadd (tmm_num, src.buf, _STRIDE);   \
}

#define init_tile_reg_and_src_with_buffer(tmm_num, src, buffer) \
{								\
  init_tile_src (tmm_num, &src, buffer);				\
  _tile_loadd (tmm_num, src.buf, _STRIDE);			\
}

/* Zero __tile src. It should be init first. */
void zero_tile_src (__tile *src)
{
  int i, j;

  for (i = 0; i < src->rows; i++)
    for (j = 0; j < src->colsb; j++)
      src->buf[i * src->colsb + j] = 0;
}

/* Zero __tilepair src. It should be init first. */
void zero_pair_tile_src (__tilepair *src)
{
  memset(src->buf, 0, 2048);
}

/* Compare tile config value with __tilecfg_u dst */
int check_tile_config (__tilecfg_u *src, __tilecfg_u *dst)
{
  size_t size = sizeof(__tilecfg);
  uint8_t *pa_src = (uint8_t *) src->a;
  uint8_t *pa_dst = (uint8_t *) dst->a;

  for (int i = 0; i < size; i++)
    if (pa_src[i] != pa_dst[i])
      return 0;

  return 1;
}

/* Compare tile register value with __tile variable */
int check_tile_register (__tile* ref, __tile* target)
{
  /* Tile register should be stored from tmm to
     memory and compare with emulation results. */
  int rows = target->rows;
  int colsb = target->colsb;
  int i, j;

  for (i = 0; i < rows; i++)
    for (j = 0; j < colsb; j++)
      if (ref->buf[i * colsb + j] != target->buf[i * colsb + j])
	return 0;

  return 1;
}

/* Compare float tile register value with __tile variable */
int check_float_tile_register (__tile* ref, __tile* target)
{
  /* Tile register should be stored from tmm to
     memory and compare with emulation results. */
  int rows = target->rows;
  int colsb = target->colsb / 4;
  int i, j;
  uint32_t *ref_buf = (uint32_t *) ref->buf;
  uint32_t *target_buf = (uint32_t *) target->buf;

  for (i = 0; i < rows; i++)
    for (j = 0; j < colsb; j++)
      if (abs(ref_buf[i * colsb + j] - target_buf[i * colsb + j]) > 1)
	return 0;

  return 1;
}

/* Compare pair_tile register value with __tile variable */
int check_pair_tile_register (__tile* ref_0, __tile* ref_1, __tilepair* target)
{
  /* Tile register should be stored from tmm to
     memory and compare with emulation results. */
  int rows = target->rows;
  int colsb = target->colsb;
  int i, j;

  for (i = 0; i < rows; i++)
    for (j = 0; j < colsb; j++)
      {
	if (ref_0->buf[i * colsb + j] != target->buf[i * colsb + j])
	  return 0;
	if (ref_1->buf[i * colsb + j] != target->buf[rows * colsb + i * colsb + j])
	  return 0;
      }

  return 1;
}

#ifndef DO_TEST
#define DO_TEST do_test
static void test_amx (void);
__attribute__ ((noinline))
static void
do_test (void)
{
  test_amx ();
}
#endif

int
main ()
{
  /* Check cpu support for AMX */
  if (__builtin_cpu_supports ("amx-tile")
#ifdef AMX_INT8
      && __builtin_cpu_supports ("amx-int8")
#endif
#ifdef AMX_BF16
      && __builtin_cpu_supports ("amx-bf16")
#endif
#ifdef AMX_FP16
      && __builtin_cpu_supports ("amx-fp16")
#endif
#ifdef AMX_COMPLEX
      && __builtin_cpu_supports ("amx-complex")
#endif
#ifdef AMX_AVX512
      && __builtin_cpu_supports ("amx-avx512")
#endif
#ifdef AMX_TF32
      && __builtin_cpu_supports ("amx-tf32")
#endif
#ifdef AMX_TRANSPOSE
      && __builtin_cpu_supports ("amx-transpose")
#endif
#ifdef AMX_FP8
      && __builtin_cpu_supports ("amx-fp8")
#endif
#ifdef AMX_MOVRS
      && __builtin_cpu_supports ("amx-movrs")
#endif
#ifdef __linux__
      && request_perm_xtile_data ()
#endif
      )
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
