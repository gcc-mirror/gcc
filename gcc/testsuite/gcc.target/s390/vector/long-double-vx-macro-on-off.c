/* { dg-do compile } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-march=z13 -mzarch" } */
#if defined(__LONG_DOUBLE_VX__)
#error
#endif

#pragma GCC target("arch=z15")
#if !defined(__LONG_DOUBLE_VX__)
#error
#endif
