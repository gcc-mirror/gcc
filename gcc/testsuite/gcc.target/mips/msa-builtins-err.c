/* Test builtins for MIPS MSA ASE instructions */
/* { dg-do compile } */
/* { dg-options "-mfp64 -mhard-float -mmsa -ffat-lto-objects" } */
/* Because this test only uses dg-error, we need to guarantee assembly
   generation ourselves by using -ffat-lto-objects. */

#include <msa.h>

v16i8 v16i8_x;
v16u8 v16u8_x;
v8i16 v8i16_x;
v8u16 v8u16_x;
v4i32 v4i32_x;
v4u32 v4u32_x;
v2i64 v2i64_x;
v2u64 v2u64_x;

volatile v16i8 v16i8_r;
volatile v16u8 v16u8_r;
volatile v8i16 v8i16_r;
volatile v8u16 v8u16_r;
volatile v4i32 v4i32_r;
volatile v4u32 v4u32_r;
volatile v2i64 v2i64_r;
volatile v2u64 v2u64_r;

/* MSA builtins with literal range of 0 to 31.  */

void
msa_add ()
{
 v16i8_r = __builtin_msa_addvi_b (v16i8_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v16i8_r = __builtin_msa_addvi_b (v16i8_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v8i16_r = __builtin_msa_addvi_h (v8i16_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v8i16_r = __builtin_msa_addvi_h (v8i16_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v4i32_r = __builtin_msa_addvi_w (v4i32_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v4i32_r = __builtin_msa_addvi_w (v4i32_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v2i64_r = __builtin_msa_addvi_d (v2i64_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v2i64_r = __builtin_msa_addvi_d (v2i64_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
}

void
msa_sub ()
{
 v16i8_r = __builtin_msa_subvi_b (v16i8_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v16i8_r = __builtin_msa_subvi_b (v16i8_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v8i16_r = __builtin_msa_subvi_h (v8i16_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v8i16_r = __builtin_msa_subvi_h (v8i16_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v4i32_r = __builtin_msa_subvi_w (v4i32_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v4i32_r = __builtin_msa_subvi_w (v4i32_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v2i64_r = __builtin_msa_subvi_d (v2i64_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v2i64_r = __builtin_msa_subvi_d (v2i64_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
}

void
msa_mini_u ()
{
 v16u8_r = __builtin_msa_mini_u_b (v16u8_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v16u8_r = __builtin_msa_mini_u_b (v16u8_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v8u16_r = __builtin_msa_mini_u_h (v8u16_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v8u16_r = __builtin_msa_mini_u_h (v8u16_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v4u32_r = __builtin_msa_mini_u_w (v4u32_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v4u32_r = __builtin_msa_mini_u_w (v4u32_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v2u64_r = __builtin_msa_mini_u_d (v2u64_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v2u64_r = __builtin_msa_mini_u_d (v2u64_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
}

void
msa_maxi_u ()
{
 v16u8_r = __builtin_msa_maxi_u_b (v16u8_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v16u8_r = __builtin_msa_maxi_u_b (v16u8_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v8u16_r = __builtin_msa_maxi_u_h (v8u16_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v8u16_r = __builtin_msa_maxi_u_h (v8u16_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v4u32_r = __builtin_msa_maxi_u_w (v4u32_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v4u32_r = __builtin_msa_maxi_u_w (v4u32_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v2u64_r = __builtin_msa_maxi_u_d (v2u64_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v2u64_r = __builtin_msa_maxi_u_d (v2u64_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
}

void
msa_clti_u ()
{
 v16i8_r = __builtin_msa_clti_u_b (v16u8_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v16i8_r = __builtin_msa_clti_u_b (v16u8_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v8i16_r = __builtin_msa_clti_u_h (v8u16_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v8i16_r = __builtin_msa_clti_u_h (v8u16_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v4i32_r = __builtin_msa_clti_u_w (v4u32_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v4i32_r = __builtin_msa_clti_u_w (v4u32_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v2i64_r = __builtin_msa_clti_u_d (v2u64_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v2i64_r = __builtin_msa_clti_u_d (v2u64_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
}

void
msa_clei_u ()
{
 v16i8_r = __builtin_msa_clei_u_b (v16u8_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v16i8_r = __builtin_msa_clei_u_b (v16u8_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v8i16_r = __builtin_msa_clei_u_h (v8u16_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v8i16_r = __builtin_msa_clei_u_h (v8u16_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v4i32_r = __builtin_msa_clei_u_w (v4u32_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v4i32_r = __builtin_msa_clei_u_w (v4u32_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v2i64_r = __builtin_msa_clei_u_d (v2u64_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v2i64_r = __builtin_msa_clei_u_d (v2u64_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
}

/* MSA builtins with literal range of -16 to 15.  */

void
msa_mini_s ()
{
 v16i8_r = __builtin_msa_mini_s_b (v16i8_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v16i8_r = __builtin_msa_mini_s_b (v16i8_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
 v8i16_r = __builtin_msa_mini_s_h (v8i16_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v8i16_r = __builtin_msa_mini_s_h (v8i16_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
 v4i32_r = __builtin_msa_mini_s_w (v4i32_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v4i32_r = __builtin_msa_mini_s_w (v4i32_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
 v2i64_r = __builtin_msa_mini_s_d (v2i64_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v2i64_r = __builtin_msa_mini_s_d (v2i64_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
}

void
msa_maxi_s ()
{
 v16i8_r = __builtin_msa_maxi_s_b (v16i8_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v16i8_r = __builtin_msa_maxi_s_b (v16i8_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
 v8i16_r = __builtin_msa_maxi_s_h (v8i16_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v8i16_r = __builtin_msa_maxi_s_h (v8i16_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
 v4i32_r = __builtin_msa_maxi_s_w (v4i32_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v4i32_r = __builtin_msa_maxi_s_w (v4i32_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
 v2i64_r = __builtin_msa_maxi_s_d (v2i64_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v2i64_r = __builtin_msa_maxi_s_d (v2i64_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
}

void
msa_ceqi ()
{
 v16i8_r = __builtin_msa_ceqi_b (v16i8_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v16i8_r = __builtin_msa_ceqi_b (v16i8_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
 v8i16_r = __builtin_msa_ceqi_h (v8i16_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v8i16_r = __builtin_msa_ceqi_h (v8i16_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
 v4i32_r = __builtin_msa_ceqi_w (v4i32_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v4i32_r = __builtin_msa_ceqi_w (v4i32_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
 v2i64_r = __builtin_msa_ceqi_d (v2i64_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v2i64_r = __builtin_msa_ceqi_d (v2i64_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
}

void
msa_clti_s ()
{
 v16i8_r = __builtin_msa_clti_s_b (v16i8_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v16i8_r = __builtin_msa_clti_s_b (v16i8_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
 v8i16_r = __builtin_msa_clti_s_h (v8i16_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v8i16_r = __builtin_msa_clti_s_h (v8i16_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
 v4i32_r = __builtin_msa_clti_s_w (v4i32_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v4i32_r = __builtin_msa_clti_s_w (v4i32_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
 v2i64_r = __builtin_msa_clti_s_d (v2i64_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v2i64_r = __builtin_msa_clti_s_d (v2i64_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
}

void
msa_clei_s ()
{
 v16i8_r = __builtin_msa_clei_s_b (v16i8_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v16i8_r = __builtin_msa_clei_s_b (v16i8_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
 v8i16_r = __builtin_msa_clei_s_h (v8i16_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v8i16_r = __builtin_msa_clei_s_h (v8i16_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
 v4i32_r = __builtin_msa_clei_s_w (v4i32_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v4i32_r = __builtin_msa_clei_s_w (v4i32_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
 v2i64_r = __builtin_msa_clei_s_d (v2i64_x, -17); /* { dg-error "must be a constant in range -16 to 15" } */
 v2i64_r = __builtin_msa_clei_s_d (v2i64_x, 16); /* { dg-error "must be a constant in range -16 to 15" } */
}

/* MSA builtins with literal range of 0 to 7/15/31/63 for
   byte/halfwords/words/doublewords elements, respectively.  */

void
msa_slli ()
{
 v16i8_r = __builtin_msa_slli_b (v16i8_x, -1); /* { dg-error "must be a constant in range 0 to 7" } */
 v16i8_r = __builtin_msa_slli_b (v16i8_x, 8); /* { dg-error "must be a constant in range 0 to 7" } */
 v8i16_r = __builtin_msa_slli_h (v8i16_x, -1); /* { dg-error "must be a constant in range 0 to 15" } */
 v8i16_r = __builtin_msa_slli_h (v8i16_x, 16); /* { dg-error "must be a constant in range 0 to 15" } */
 v4i32_r = __builtin_msa_slli_w (v4i32_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v4i32_r = __builtin_msa_slli_w (v4i32_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v2i64_r = __builtin_msa_slli_d (v2i64_x, -1); /* { dg-error "must be a constant in range 0 to 63" } */
 v2i64_r = __builtin_msa_slli_d (v2i64_x, 64); /* { dg-error "must be a constant in range 0 to 63" } */
}

void
msa_srai ()
{
 v16i8_r = __builtin_msa_srai_b (v16i8_x, -1); /* { dg-error "must be a constant in range 0 to 7" } */
 v16i8_r = __builtin_msa_srai_b (v16i8_x, 8); /* { dg-error "must be a constant in range 0 to 7" } */
 v8i16_r = __builtin_msa_srai_h (v8i16_x, -1); /* { dg-error "must be a constant in range 0 to 15" } */
 v8i16_r = __builtin_msa_srai_h (v8i16_x, 16); /* { dg-error "must be a constant in range 0 to 15" } */
 v4i32_r = __builtin_msa_srai_w (v4i32_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v4i32_r = __builtin_msa_srai_w (v4i32_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v2i64_r = __builtin_msa_srai_d (v2i64_x, -1); /* { dg-error "must be a constant in range 0 to 63" } */
 v2i64_r = __builtin_msa_srai_d (v2i64_x, 64); /* { dg-error "must be a constant in range 0 to 63" } */
}

void
msa_srli ()
{
 v16i8_r = __builtin_msa_srli_b (v16i8_x, -1); /* { dg-error "must be a constant in range 0 to 7" } */
 v16i8_r = __builtin_msa_srli_b (v16i8_x, 8); /* { dg-error "must be a constant in range 0 to 7" } */
 v8i16_r = __builtin_msa_srli_h (v8i16_x, -1); /* { dg-error "must be a constant in range 0 to 15" } */
 v8i16_r = __builtin_msa_srli_h (v8i16_x, 16); /* { dg-error "must be a constant in range 0 to 15" } */
 v4i32_r = __builtin_msa_srli_w (v4i32_x, -1); /* { dg-error "must be a constant in range 0 to 31" } */
 v4i32_r = __builtin_msa_srli_w (v4i32_x, 32); /* { dg-error "must be a constant in range 0 to 31" } */
 v2i64_r = __builtin_msa_srli_d (v2i64_x, -1); /* { dg-error "must be a constant in range 0 to 63" } */
 v2i64_r = __builtin_msa_srli_d (v2i64_x, 64); /* { dg-error "must be a constant in range 0 to 63" } */
}

/* MSA builtins with literal range of 0 to 15/7/3/1 for
   byte/halfwords/words/doublewords elements, respectively.  */

void
msa_insert (int a)
{
 v16i8_r = __builtin_msa_insert_b (v16i8_x, -1, a); /* { dg-error "must be a constant in range 0 to 15" } */
 v16i8_r = __builtin_msa_insert_b (v16i8_x, 16, a); /* { dg-error "must be a constant in range 0 to 15" } */
 v8i16_r = __builtin_msa_insert_h (v8i16_x, -1, a); /* { dg-error "must be a constant in range 0 to 7" } */
 v8i16_r = __builtin_msa_insert_h (v8i16_x, 8, a); /* { dg-error "must be a constant in range 0 to 7" } */
 v4i32_r = __builtin_msa_insert_w (v4i32_x, -1, a); /* { dg-error "must be a constant in range 0 to 3" } */
 v4i32_r = __builtin_msa_insert_w (v4i32_x, 4, a); /* { dg-error "must be a constant in range 0 to 3" } */
 v2i64_r = __builtin_msa_insert_d (v2i64_x, -1, a); /* { dg-error "must be a constant in range 0 to 1" } */
 v2i64_r = __builtin_msa_insert_d (v2i64_x, 2, a); /* { dg-error "must be a constant in range 0 to 1" } */
}

void
msa_insve ()
{
 v16i8_r = __builtin_msa_insve_b (v16i8_x, -1, v16i8_x); /* { dg-error "must be a constant in range 0 to 15" } */
 v16i8_r = __builtin_msa_insve_b (v16i8_x, 16, v16i8_x); /* { dg-error "must be a constant in range 0 to 15" } */
 v8i16_r = __builtin_msa_insve_h (v8i16_x, -1, v8i16_x); /* { dg-error "must be a constant in range 0 to 7" } */
 v8i16_r = __builtin_msa_insve_h (v8i16_x, 8, v8i16_x); /* { dg-error "must be a constant in range 0 to 7" } */
 v4i32_r = __builtin_msa_insve_w (v4i32_x, -1, v4i32_x); /* { dg-error "must be a constant in range 0 to 3" } */
 v4i32_r = __builtin_msa_insve_w (v4i32_x, 4, v4i32_x); /* { dg-error "must be a constant in range 0 to 3" } */
 v2i64_r = __builtin_msa_insve_d (v2i64_x, -1, v2i64_x); /* { dg-error "must be a constant in range 0 to 1" } */
 v2i64_r = __builtin_msa_insve_d (v2i64_x, 2, v2i64_x); /* { dg-error "must be a constant in range 0 to 1" } */
}
