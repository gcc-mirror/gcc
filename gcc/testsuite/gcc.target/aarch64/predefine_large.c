/* { dg-skip-if "Code model already defined" { aarch64_tiny || aarch64_small } } */

#ifdef __AARCH64_CMODEL_LARGE__
  int dummy;
#else
  #error
#endif
