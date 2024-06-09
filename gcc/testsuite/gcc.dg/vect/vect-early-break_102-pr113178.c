/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-std=gnu99 -fpermissive -fgnu89-inline -Ofast -fprofile-generate -w" } */

extern int replace_reg_with_saved_mem_i, replace_reg_with_saved_mem_nregs,
    replace_reg_with_saved_mem_mem_1;
replace_reg_with_saved_mem_mode() {
  if (replace_reg_with_saved_mem_i)
    return;
  while (++replace_reg_with_saved_mem_i < replace_reg_with_saved_mem_nregs)
    if (replace_reg_with_saved_mem_i)
      break;
  if (replace_reg_with_saved_mem_i)
    if (replace_reg_with_saved_mem_mem_1)
      adjust_address_1();
  replace_reg_with_saved_mem_mem_1 ? fancy_abort() : 0;
}
