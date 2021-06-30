/* Test __NO_SIGNED_ZEROS__ is defined with -fno-signed-zeros.  */
/* { dg-do compile } */
/* { dg-options "-fno-signed-zeros" } */

#ifndef __NO_SIGNED_ZEROS__
#error "__NO_SIGNED_ZEROS__ not defined"
#endif

#pragma GCC optimize "-fsigned-zeros"
#ifdef __NO_SIGNED_ZEROS__
#error "__NO_SIGNED_ZEROS__ defined"
#endif

#pragma GCC optimize "-fno-signed-zeros"
#ifndef __NO_SIGNED_ZEROS__
#error "__NO_SIGNED_ZEROS__ not defined"
#endif
