/* { dg-skip-if "Code model already defined" { aarch64_tiny || aarch64_large } } */

#ifdef __AARCH64_CMODEL_SMALL__
  int dummy;
#else
  #error
#endif
