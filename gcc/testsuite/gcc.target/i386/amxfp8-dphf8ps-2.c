/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target amx_fp8 } */
/* { dg-options "-O2 -mamx-fp8" } */

#define AMX_FP8
#define DO_TEST test_amx_fp8_dphf8ps
void test_amx_fp8_dphf8ps ();

#include "amx-helper.h"
#include "fp8-helper.h"
#include "fp-emulation.h"

void 
calc_matrix_dphf8ps (__tile *dst, __tile *src1, __tile *src2)
{
  unsigned char *src1_buf = (unsigned char *)src1->buf;
  unsigned char *src2_buf = (unsigned char *)src2->buf;
  float *dst_buf = (float *)dst->buf;

  int M = src1->rows;
  int N = src1->colsb / 4;
  int K = src2->colsb / 4;
  
  int* valueState_a = (int*)malloc(sizeof(int));
  int* valueState_b = (int*)malloc(sizeof(int));
  int* valueState = (int *)malloc(M * K * sizeof(int));
  __int128_t *temp = (__int128_t *)malloc(M * K * sizeof(__int128_t));

  for (int i = 0; i < M; i++)
    {
      for (int j = 0; j < N; j++)
        for (int k = 0; k < K; k++)
          {
            int64_t s1e0 = shift_fp8_to_int64(src1_buf[4 * i * N + 4 * j + 0], 0, valueState_a);
            int64_t s2e0 = shift_fp8_to_int64(src2_buf[4 * j * K + 4 * k + 0], 0, valueState_b);
	    int valueState_e0 = state_handler(*valueState_a, *valueState_b, s1e0, s2e0, '*');

            int64_t s1e1 = shift_fp8_to_int64(src1_buf[4 * i * N + 4 * j + 1], 0, valueState_a);
            int64_t s2e1 = shift_fp8_to_int64(src2_buf[4 * j * K + 4 * k + 1], 0, valueState_b);
	    int valueState_e1 = state_handler(*valueState_a, *valueState_b, s1e1, s2e1, '*');
	    
            int64_t s1e2 = shift_fp8_to_int64(src1_buf[4 * i * N + 4 * j + 2], 0, valueState_a);
            int64_t s2e2 = shift_fp8_to_int64(src2_buf[4 * j * K + 4 * k + 2], 0, valueState_b);
	    int valueState_e2 = state_handler(*valueState_a, *valueState_b, s1e2, s2e2, '*');

            int64_t s1e3 = shift_fp8_to_int64(src1_buf[4 * i * N + 4 * j + 3], 0, valueState_a);
            int64_t s2e3 = shift_fp8_to_int64(src2_buf[4 * j * K + 4 * k + 3], 0, valueState_b);
	    int valueState_e3 = state_handler(*valueState_a, *valueState_b, s1e3, s2e3, '*');

	    valueState[i * K + k] = state_handler(
				      state_handler(
					state_handler(
					  state_handler(
			valueState_e0, valueState_e1, (__int128_t)s1e0 * s2e0, s1e1 * s2e1, '+'),
			valueState_e2, (__int128_t)s1e0 * s2e0 + s1e1 * s2e1, s1e2 * s2e2, '+'), 
			valueState_e3, (__int128_t)s1e0 * s2e0 + s1e1 * s2e1 + s1e2 * s2e2, s1e3 * s2e3, '+'),
			valueState[i * K + k], (__int128_t)s1e0 * s2e0 + s1e1 * s2e1 + s1e2 * s2e2 + s1e3 * s2e3, temp[i * K + k], '+');

	    temp[i * K + k] += (__int128_t)s1e0 * (__int128_t)s2e0 
	    			+ (__int128_t)s1e1 * (__int128_t)s2e1 
				+ (__int128_t)s1e2 * (__int128_t)s2e2 
				+ (__int128_t)s1e3 * (__int128_t)s2e3;
          }
    }

  for (int i = 0; i < M; i++)
    for (int k = 0; k < K; k++)
      {
	float tmp_float = shift_int128_to_fp32(temp[i * K + k], 0, 0);
	if (isnan(dst_buf[i * K + k]) || isnan(tmp_float))
	  dst_buf[i * K + k] = -nanf("");
	else switch(valueState[i * K + k]) 
	  {
	    case -3:
	      dst_buf[i * K + k] = -INFINITY;
	      break;
	    case 3:
	      dst_buf[i * K + k] = 1.0 / 0.0;
	      break;
	    case 2:
	    case -2:
	      dst_buf[i * K + k] = -nanf("");
	      break;
	    case -1:
	    case 0:
	    case 1:
	    default:
	      dst_buf[i * K + k] += tmp_float;
	      break;
	    }
      }

  free(valueState_a);
  free(valueState_b);
  free(valueState);
  free(temp);
}

void test_amx_fp8_dphf8ps ()
{
  __tilecfg_u cfg;
  __tile dst, dst_ref, src1, src2;
  uint8_t tmp_dst_zero_buf[1024], tmp_src_buf[1024];

  init_fp32_max_tile_zero_buffer (tmp_dst_zero_buf);
  init_fp8_max_tile_buffer(tmp_src_buf);

  init_tile_config (&cfg);
  init_tile_reg_and_src_with_buffer (1, dst, tmp_dst_zero_buf);
  init_tile_reg_and_src_with_buffer (2, src1, tmp_src_buf);
  init_tile_reg_and_src_with_buffer (3, src2, tmp_src_buf);

  calc_matrix_dphf8ps (&dst, &src1, &src2);

  _tile_dphf8ps (1, 2, 3);
  _tile_stored (1, dst_ref.buf, _STRIDE);

  if (!check_float_tile_register (&dst_ref, &dst))
    abort ();
}
