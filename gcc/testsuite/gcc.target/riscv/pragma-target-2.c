/* Test for #pragma GCC target with tune parameter */
/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=rocket -O2" } */

void default_tune(void) {
  /* Default tune is rocket */
}

#pragma GCC push_options
#pragma GCC target("tune=sifive-7-series")
void sifive_tune(void) {
  /* Tune should be sifive-7-series */
}
#pragma GCC pop_options

void back_to_rocket(void) {
  /* Tune should be back to rocket */
}

#pragma GCC target("arch=rv64gcv;tune=generic")
void combined_options(void) {
#ifndef __riscv_vector
  __builtin_abort();  /* Should have vector */
#endif
  /* Tune should be generic */
}
