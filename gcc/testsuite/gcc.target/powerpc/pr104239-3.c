/* PR target/104239 */
/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mvsx -DNO_WARN_X86_INTRINSICS -std=c89" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <x86intrin.h>

int i;
