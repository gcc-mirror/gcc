/* { dg-require-visibility "" } */
/* { dg-options "-fvisibility=internal -fPIC" { target fpic } } */
/* { dg-additional-options "-std=gnu17" } */

extern char num_4x4_blocks_high_lookup, num_4x4_blocks_wide_lookup,
    vp9_foreach_transformed_block_in_plane_xd_0_0_0;
int vp9_first_pass_encode_tile_mb_row_cpi_0,
    vp9_first_pass_encode_tile_mb_row_tile_data_0_0, vp9_encode_sby_pass1_x;
typedef void foreach_transformed_block_visitor();
void vp9_encode_sby_pass1();
typedef struct {
  long coded_error;
  long sr_coded_error;
  long frame_noise_energy;
  long intra_error;
} FIRSTPASS_DATA;
typedef struct {
  FIRSTPASS_DATA fp_data;
} TileDataEnc;
TileDataEnc accumulate_fp_mb_row_stat_this_tile;
void vp9_first_pass_encode_tile_mb_row(FIRSTPASS_DATA *fp_acc_data) {
  int mb_col_end = vp9_first_pass_encode_tile_mb_row_tile_data_0_0;
  for (; mb_col_end;) {
    vp9_encode_sby_pass1();
    if (vp9_first_pass_encode_tile_mb_row_cpi_0) {
      FIRSTPASS_DATA __trans_tmp_1 = *fp_acc_data;
      accumulate_fp_mb_row_stat_this_tile.fp_data.coded_error +=
          accumulate_fp_mb_row_stat_this_tile.fp_data.sr_coded_error +=
          __trans_tmp_1.sr_coded_error;
      accumulate_fp_mb_row_stat_this_tile.fp_data.frame_noise_energy +=
          __trans_tmp_1.frame_noise_energy;
      accumulate_fp_mb_row_stat_this_tile.fp_data.intra_error +=
          __trans_tmp_1.intra_error;
    }
  }
}
foreach_transformed_block_visitor vp9_foreach_transformed_block_in_plane_visit;
void vp9_foreach_transformed_block_in_plane(void *arg) {
  int c, max_blocks_wide = num_4x4_blocks_wide_lookup;
  for (; num_4x4_blocks_high_lookup;) {
    c = 0;
    for (; c < max_blocks_wide;
         c += vp9_foreach_transformed_block_in_plane_xd_0_0_0)
      vp9_foreach_transformed_block_in_plane_visit(arg);
  }
}
void vp9_encode_sby_pass1() {
  vp9_foreach_transformed_block_in_plane(&vp9_encode_sby_pass1_x);
}
