/* { dg-do compile } */
/* { dg-additional-options "-fprofile-generate" } */
/* { dg-additional-options "-mavx512vl" { target x86_64-*-* i?86-*-* } } */

int *mask_slp_int64_t_8_2_x, *mask_slp_int64_t_8_2_y, *mask_slp_int64_t_8_2_z;

void
__attribute__mask_slp_int64_t_8_2() {
  for (int i; i; i += 8) {
    mask_slp_int64_t_8_2_x[i + 6] =
        mask_slp_int64_t_8_2_y[i + 6] ? mask_slp_int64_t_8_2_z[i] : 1;
    mask_slp_int64_t_8_2_x[i + 7] =
        mask_slp_int64_t_8_2_y[i + 7] ? mask_slp_int64_t_8_2_z[i + 7] : 2;
  }
}
