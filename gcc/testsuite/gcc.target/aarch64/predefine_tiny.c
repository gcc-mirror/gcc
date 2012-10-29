/* { dg-skip-if "Code model already defined" { aarch64_small || aarch64_large } } */

#ifdef __AARCH64_CMODEL_TINY__
  int dummy;
#else
  #error
#endif
