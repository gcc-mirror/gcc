/* { dg-do compile } */
/* { dg-additional-options "-mavx512bw -mtune-ctrl=avx512_masked_epilogues" } */

void test (const unsigned char * __restrict__ pi,
	   const float * __restrict__ blk,
	   int texel_count,
	   float *pp_avg_rgb)
{
    float pp_avg_rgb_0 = 0.0f;
    float pp_avg_rgb_1 = 0.0f;
    float pp_avg_rgb_2 = 0.0f;
    float pp_avg_rgb_3 = 0.0f;
    for (int lane_id = 0; lane_id < texel_count; lane_id++) {
        unsigned char r_byte = pi[lane_id * 4 + 0];
        unsigned char g_byte = pi[lane_id * 4 + 1];
        unsigned char b_byte = pi[lane_id * 4 + 2];
        unsigned char a_byte = pi[lane_id * 4 + 3];

        float r_float = blk[lane_id * 4 + 0];
        float g_float = blk[lane_id * 4 + 1];
        float b_float = blk[lane_id * 4 + 2];
        float a_float = blk[lane_id * 4 + 3];

        int r_is_zero = (r_byte == 0) ? 1 : 0;
        int r_in_bounds = (texel_count > lane_id) ? 1 : 0;
        int r_mask = r_is_zero * (-r_in_bounds);
        if (r_mask != 0) {
            pp_avg_rgb_0 += r_float;
        }
        int g_is_zero = (g_byte == 0) ? 1 : 0;
        int g_in_bounds = (texel_count > lane_id) ? 1 : 0;
        int g_mask = g_is_zero * (-g_in_bounds);
        if (g_mask != 0) {
            pp_avg_rgb_1 += g_float;
        }
        int b_is_zero = (b_byte == 0) ? 1 : 0;
        int b_in_bounds = (texel_count > lane_id) ? 1 : 0;
        int b_mask = b_is_zero * (-b_in_bounds);
        if (b_mask != 0) {
            pp_avg_rgb_2 += b_float;
        }
        int a_is_zero = (a_byte == 0) ? 1 : 0;
        int a_in_bounds = (texel_count > lane_id) ? 1 : 0;
        int a_mask = a_is_zero * (-a_in_bounds);
        if (a_mask != 0) {
            pp_avg_rgb_3 += a_float;
        }
    }
    pp_avg_rgb[0] = pp_avg_rgb_0;
    pp_avg_rgb[1] = pp_avg_rgb_1;
    pp_avg_rgb[2] = pp_avg_rgb_2;
    pp_avg_rgb[3] = pp_avg_rgb_3;
}

/* Even though there's an SLP opportunity in-order reductions should never use
   masked epilogs.  */
/* { dg-final { scan-tree-dump "optimized: loop vectorized using 64 byte vectors" "vect" } } */
/* { dg-final { scan-tree-dump "optimized: epilogue loop vectorized using 32 byte vectors" "vect" } } */
