/* PR target/109137 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O3 -march=znver1 -mfpmath=sse -w" } */
/* { dg-additional-options "-fPIC" { target fpic } } */

typedef struct {
  char bytestream_end;
} CABACContext;
int get_cabac___trans_tmp_3, get_cabac_tmp, get_cabac_c,
    decode_cabac_mb_intra4x4_pred_mode_mode, ff_h264_decode_mb_cabac_h_0,
    ff_h264_decode_mb_cabac_bit;
typedef struct {
  char intra4x4_pred_mode_cache[2];
} H264SliceContext;
H264SliceContext ff_h264_decode_mb_cabac_sl;
void ff_h264_decode_mb_cabac(void) {
  __builtin_memset((void*)ff_h264_decode_mb_cabac_h_0, 6, 48);
  int i;
  for (;; i++) {
    __asm__(""/* { dg-error "'asm' operand has impossible constraints" } */
            : "=&r"(ff_h264_decode_mb_cabac_bit), "=&r"(get_cabac_c),
              "=&r"(get_cabac_c), "=&q"(get_cabac_tmp)
            : "r"(get_cabac___trans_tmp_3),
              "r"(__builtin_offsetof(CABACContext, bytestream_end))
            : "ecx");
    ff_h264_decode_mb_cabac_sl.intra4x4_pred_mode_cache[i] =
        decode_cabac_mb_intra4x4_pred_mode_mode;
  }
}
