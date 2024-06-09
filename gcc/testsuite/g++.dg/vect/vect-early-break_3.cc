/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-w -O2" } */

int aarch64_advsimd_valid_immediate_hs_val32;
bool aarch64_advsimd_valid_immediate_hs() {
  for (int shift = 0; shift < 32; shift += 8)
    if (aarch64_advsimd_valid_immediate_hs_val32 & shift)
      return aarch64_advsimd_valid_immediate_hs_val32;
  for (;;)
    ;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
