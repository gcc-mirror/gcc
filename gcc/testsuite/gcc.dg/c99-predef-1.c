/* Verify that predefined macros for properties of the compiler and
   library together are the same before and after system headers are
   included.  This is broken with older glibc versions.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

#ifdef __STDC_IEC_559__
#define IEC_559_DEFINED_BEFORE 1
#else
#define IEC_559_DEFINED_BEFORE 0
#endif

#ifdef __STDC_IEC_559_COMPLEX__
#define IEC_559_COMPLEX_DEFINED_BEFORE 1
#else
#define IEC_559_COMPLEX_DEFINED_BEFORE 0
#endif

#ifdef __STDC_ISO_10646__
#define ISO_10646_DEFINED_BEFORE 1
#else
#define ISO_10646_DEFINED_BEFORE 0
#endif

#include <stdio.h>

#ifdef __STDC_IEC_559__
#define IEC_559_DEFINED_AFTER 1
#else
#define IEC_559_DEFINED_AFTER 0
#endif

#ifdef __STDC_IEC_559_COMPLEX__
#define IEC_559_COMPLEX_DEFINED_AFTER 1
#else
#define IEC_559_COMPLEX_DEFINED_AFTER 0
#endif

#ifdef __STDC_ISO_10646__
#define ISO_10646_DEFINED_AFTER 1
#else
#define ISO_10646_DEFINED_AFTER 0
#endif

#if defined(__GLIBC__) && (__GLIBC__ < 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ < 16))
#define BROKEN
#endif

#ifndef BROKEN

#if IEC_559_DEFINED_BEFORE != IEC_559_DEFINED_AFTER
#error "__STDC_IEC_559__ definition inconsistency"
#endif

#if IEC_559_COMPLEX_DEFINED_BEFORE != IEC_559_COMPLEX_DEFINED_AFTER
#error "__STDC_IEC_559_COMPLEX__ definition inconsistency"
#endif

#if ISO_10646_DEFINED_BEFORE != ISO_10646_DEFINED_AFTER
#error "__STDC_ISO_10646__ definition inconsistency"
#endif

#endif
