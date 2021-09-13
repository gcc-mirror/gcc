/* { dg-do compile } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-march=z14 -mzarch" } */
#if !defined(__LONG_DOUBLE_VX__)
#error
#endif

#pragma GCC target("arch=zEC12")
#if defined(__LONG_DOUBLE_VX__)
#error
#endif
