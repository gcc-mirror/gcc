/* { dg-additional-options "-fdump-analyzer" } */

enum { PIPE_SWIZZLE_X, PIPE_SWIZZLE_Z, PIPE_SWIZZLE_W };
enum { UTIL_FORMAT_COLORSPACE_YUV, UTIL_FORMAT_COLORSPACE_ZS } colorspace;
int util_format_colormask_descutil_format_colormask_colormask,
    util_format_get_component_bits_component,
    util_format_get_component_bits_desc_0_3_0;
void util_format_colormask_descutil_format_colormask() {
  switch (colorspace) {
  case UTIL_FORMAT_COLORSPACE_YUV:
    util_format_colormask_descutil_format_colormask_colormask = 0;
    for (;;)
    case UTIL_FORMAT_COLORSPACE_ZS:; /* { dg-warning "infinite loop" } */
  }
}
unsigned util_format_get_component_bits() {
  switch (util_format_get_component_bits_component)
  case PIPE_SWIZZLE_X:
  case PIPE_SWIZZLE_W:
    return util_format_get_component_bits_desc_0_3_0;
} /* { dg-warning "use of uninitialized value '<return-value>'" "" { xfail *-*-* } } */
