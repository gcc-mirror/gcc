/* { dg-do compile } */

enum { SEG_THIN_POOL } read_only;
struct {
  unsigned skip_block_zeroing;
  unsigned ignore_discard;
  unsigned no_discard_passdown;
  unsigned error_if_no_space;
} _thin_pool_emit_segment_line_seg;
void dm_snprintf();
void _emit_segment()
{
  int features =
      (_thin_pool_emit_segment_line_seg.error_if_no_space ? 1 : 0) +
      (read_only ? 1 : 0) +
      (_thin_pool_emit_segment_line_seg.ignore_discard ? 1 : 0) +
      (_thin_pool_emit_segment_line_seg.no_discard_passdown ? 1 : 0) +
      (_thin_pool_emit_segment_line_seg.skip_block_zeroing ? 1 : 0);
  dm_snprintf(features);
}
