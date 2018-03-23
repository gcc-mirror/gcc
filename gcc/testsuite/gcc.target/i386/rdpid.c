/* { dg-do compile } */
/* { dg-options "-O2 -mrdpid" } */
/* { dg-final { scan-assembler "rdpid" } } */

#include <x86intrin.h>

unsigned int
read_rdpid32 (void)
{
  return _rdpid_u32 ();
}
