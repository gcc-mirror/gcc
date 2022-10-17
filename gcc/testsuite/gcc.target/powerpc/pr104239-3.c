/* PR target/104239 */
/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -DNO_WARN_X86_INTRINSICS -std=c89" } */
/* { dg-require-effective-target powerpc_p8vector_ok } */

#include <x86intrin.h>

int i;
