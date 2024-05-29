/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O3 -mdejagnu-cpu=power7 -std=gnu89" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Make sure that the conditional macros vector, bool, and pixel are not
   considered as being defined.  */

#ifdef bool
#error "bool is considered defined"
#endif

#ifdef vector
#error "vector is considered defined"
#endif

#ifdef pixel
#error "pixel is condsidered defined"
#endif

#if defined(bool)
#error "bool is considered defined"
#endif

#if defined(vector)
#error "vector is considered defined"
#endif

#if defined(pixel)
#error "pixel is condsidered defined"
#endif

#ifndef bool
#else
#error "bool is considered defined"
#endif

#ifndef vector
#else
#error "vector is considered defined"
#endif

#ifndef pixel
#else
#error "pixel is condsidered defined"
#endif

#define bool long double
bool pixel = 0;
