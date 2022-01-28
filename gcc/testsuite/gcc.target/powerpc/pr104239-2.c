/* PR target/104239 */
/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -DNO_WARN_X86_INTRINSICS" } */
/* { dg-require-effective-target powerpc_p8vector_ok } */

#if __has_include(<x86gprintrin.h>)
#include <x86gprintrin.h>
#endif

int i;
